module C_Expressions where

import Data.List as List
import Data.Map as Map

import Control.Monad.Reader
import Control.Monad.State

import Latte.Abs

import C_Utils


----------------------
-- helper functions --
----------------------
getVariableMapValue :: Variable -> CMonad (Memory, Type)
getVariableMapValue variableName = do
    environment <- ask
    case Map.lookup variableName (variableMap environment) of
        Nothing ->
            throwCMonad $
                "Impossible error: existence of variable `" ++
                variableName ++
                "` has already been confirmed by the typechecker"
        (Just memoryAndType) ->
            return memoryAndType

getFunctionReturnTypeMapValue :: Variable -> CMonad Type
getFunctionReturnTypeMapValue functionName = do
    functionReturnTypeMap <- gets functionReturnTypeMap
    case Map.lookup functionName functionReturnTypeMap of
        Nothing ->
            throwCMonad $
                "Impossible error: existence of function `" ++
                functionName ++
                "` has already been confirmed by the typechecker"
        (Just returnType) ->
            return returnType

getVMTableMapValue :: Expr -> CMonad VMTable
getVMTableMapValue expression = do
    Cls (Ident className) <- getExpressionType expression
    vmtableMap            <- gets vmtableMap
    case Map.lookup className vmtableMap of
        Nothing ->
            throwCMonad $
                "Impossible error: existence of VM table in class `" ++
                className ++
                "` has already been checked by the typechecker"
        (Just vmtable) ->
            return vmtable

getVMTableAttributeMapValue :: Expr -> Variable -> CMonad (Memory, Type)
getVMTableAttributeMapValue expression attributeName = do
    virtualMethodTable <- getVMTableMapValue expression
    case Map.lookup attributeName (virtualAttributeMap virtualMethodTable) of
        Nothing ->
            throwCMonad $
                "Impossible error: existence of attribute `" ++
                attributeName ++
                "` has already been checked by the typechecker"
        (Just memoryAndType) ->
            return memoryAndType

getVMTableMethodListValue :: Expr -> Variable -> CMonad (Type, Integer, Variable)
getVMTableMethodListValue expression methodName = do
    virtualMethodTable <- getVMTableMapValue expression
    case Map.lookup methodName (Map.fromList $ virtualMethodList virtualMethodTable) of
        Nothing ->
            throwCMonad $
                "Impossible error: existence of method `" ++
                methodName ++
                "` has already been checked by the typechecker"
        (Just typeIntegerVariable) ->
            return typeIntegerVariable

translateBinaryOperator :: [Instruction] -> InstructionPrepend -> InstructionPrepend
translateBinaryOperator operatorInstructionList returnCode =
    instructionListMerge ((POP $ RegisterOp ECX) : operatorInstructionList) .
    returnCode

translateBinaryOperands :: Expr -> Expr -> InstructionPrepend -> CMonad InstructionPrepend
translateBinaryOperands expressionL expressionR operandCode = do
    codeL <- translateExpression expressionL
    codeR <- translateExpression expressionR
    return $
        codeL .
        instructionListAdd (PUSH $ RegisterOp EAX) .
        codeR .
        operandCode .
        instructionListAdd (MOV (RegisterOp ECX) (RegisterOp EAX))

translateExpressionEVar :: Variable -> (Memory -> [Instruction]) -> (Memory -> [Instruction]) -> CMonad InstructionPrepend
translateExpressionEVar variable attributeCode localCode = do
    (loc, _) <- getVariableMapValue variable
    environment <- ask
    case loc of
        Attribute _ ->
            return $ instructionListMerge (attributeCode loc)
        _ ->
            return $ instructionListMerge (localCode loc)

-----------------------
-- L-value functions --
-----------------------
translateLValue :: Expr -> CMonad InstructionPrepend
translateLValue (EVar (Ident identifier)) = do
    translateExpressionEVar identifier attributeCode localCode
    where
        attributeCode :: Memory -> [Instruction]
        attributeCode loc = [
            MOV (MemoryOp $ Parameter 1) (RegisterOp EAX),
            LEA (MemoryOp loc) (RegisterOp EDX),
            MOV (RegisterOp EDX) (RegisterOp EAX)
            ]

        localCode :: Memory -> [Instruction]
        localCode loc = [
            LEA (MemoryOp loc) (RegisterOp EAX)
            ]

translateLValue (EArr expressionL expressionR) = do
    generatedCodeL <- translateLValue expressionL
    generatedCodeR <- translateExpression expressionR
    return $
        generatedCodeR .
        instructionListAdd (PUSH (RegisterOp EAX)) .
        generatedCodeL .
        generateCode
    where
        generateCode :: InstructionPrepend
        generateCode = case expressionL of
            (EApp _ _)    -> generateAccessibleCode
            (EMeth _ _ _) -> generateAccessibleCode
            (ENew _ _)    -> generateAccessibleCode
            _             -> generateAssignableCode

        generateAccessibleCode :: InstructionPrepend
        generateAccessibleCode = instructionListMerge [
            POP (RegisterOp EDX),
            UNARY_INSTRUCTION INC (RegisterOp EDX),
            LEA (ArrayElementAddressOp EAX EDX dword) (RegisterOp EAX)
            ]

        generateAssignableCode :: InstructionPrepend
        generateAssignableCode = instructionListMerge [
            POP (RegisterOp EDX),
            UNARY_INSTRUCTION INC (RegisterOp EDX),
            MOV (AddressOp 0 EAX) (RegisterOp EAX),
            LEA (ArrayElementAddressOp EAX EDX dword) (RegisterOp EAX)
            ]

translateLValue (EAttr expression (Ident identifier)) = do
    accumulatedCode      <- translateLValue expression
    (Attribute value, _) <- getVMTableAttributeMapValue expression identifier
    return $
        accumulatedCode .
        generateCode value
    where
        generateCode :: Integer -> InstructionPrepend
        generateCode value = do
            case expression of
                (EApp _ _)    -> generateAccessibleCode value
                (EMeth _ _ _) -> generateAccessibleCode value
                (ENew _ _)    -> generateAccessibleCode value
                _             -> generateAssignableCode value

        generateAccessibleCode :: Integer -> InstructionPrepend
        generateAccessibleCode value =
            instructionListAdd (LEA (AttributeAddressOp value EAX) (RegisterOp EAX))

        generateAssignableCode :: Integer -> InstructionPrepend
        generateAssignableCode value =
            instructionListMerge [
                MOV (AddressOp 0 EAX) (RegisterOp EDX),
                LEA (AttributeAddressOp value EDX) (RegisterOp EAX)
            ]

translateLValue (EApp (Ident identifier) expressionList) = do
    insideClass <- asks insideClass
    case insideClass of
        Nothing -> do
            translateLValueEApp
        (Just className) -> do
            vmtableMap <- gets vmtableMap
            case Map.lookup className vmtableMap of
                Nothing ->
                    throwCMonad $
                        "Impossible error: existence of class `" ++
                        className ++
                        "` has already been confirmed by the typechecker"
                (Just virtualMethodTable) -> do
                    case Map.lookup identifier (Map.fromList $ virtualMethodList virtualMethodTable) of
                        Nothing -> do
                            translateLValueEApp
                        (Just _) -> do
                            translateLValue (EMeth (EVar (Ident "self")) (Ident identifier) expressionList)
    where
        translateLValueEApp :: CMonad InstructionPrepend
        translateLValueEApp = do
            expressionListCodeList <- mapM translateExpression expressionList
            let expressionListCode =
                    List.foldr
                        (\code accumulatedCode -> code . instructionListAdd (PUSH $ RegisterOp EAX) . accumulatedCode)
                        id
                        (reverse expressionListCodeList)
            returnType <- getFunctionReturnTypeMapValue identifier
            return $
                expressionListCode .
                instructionListMerge [
                    CALL identifier,
                    BINARY_INSTRUCTION ADD (LiteralOp (dword * fromIntegral (length expressionList))) (RegisterOp ESP)
                ]


translateLValue (EMeth expression (Ident identifier) expressionList) = do
    (Fun functionType _, offset, _) <- getVMTableMethodListValue expression identifier
    expressionCode         <- translateExpression expression
    expressionListCodeList <- mapM translateExpression expressionList
    let expressionListCode =
            List.foldr
                (\code accumulatedCode -> code . instructionListAdd (PUSH $ RegisterOp EAX) . accumulatedCode)
                id
                (reverse expressionListCodeList)
    return $
        expressionListCode .
        expressionCode .
        instructionListMerge [
            PUSH $ RegisterOp EAX,
            MOV (AddressOp 0 EAX) (RegisterOp EAX),
            CALL_METHOD (MethodAddressOp offset EAX),
            BINARY_INSTRUCTION ADD (LiteralOp (dword * (1 + fromIntegral (length expressionList)))) (RegisterOp ESP)
        ]

translateLValue (ENew _ (EArrLen expression)) = do
    expressionCode <- translateExpression expression
    return $
        expressionCode .
        instructionListMerge [
            PUSH $ RegisterOp EAX,
            UNARY_INSTRUCTION INC (RegisterOp EAX),
            PUSH $ LiteralOp dword,
            PUSH $ RegisterOp EAX,
            CALL "calloc",
            BINARY_INSTRUCTION ADD (LiteralOp (dword * 2)) (RegisterOp ESP),
            POP (RegisterOp EDX),
            MOV (RegisterOp EDX) (AddressOp 0 EAX)
        ]

translateLValue eNew@(ENew (Cls (Ident identifier)) EClsLen) = do
    virtualMethodTable <- getVMTableMapValue eNew
    let memorySize = (1 + ) $ fromIntegral $ Map.size $ virtualAttributeMap virtualMethodTable
    let generatedCode =
            if (virtualMethodList virtualMethodTable) /= []
                then
                    instructionListAdd $ MOV (VMTableLiteralOp $ getVirtualMethodTableLabel identifier) (AddressOp 0 EAX)
                else
                    id
    return $
        instructionListMerge [
            PUSH $ LiteralOp dword,
            PUSH $ LiteralOp memorySize,
            CALL "calloc",
            BINARY_INSTRUCTION ADD (LiteralOp (dword * 2)) (RegisterOp ESP)
        ] .
        generatedCode

----------------------------
-- jumping code functions --
----------------------------
translateCondition :: Expr -> Integer -> Integer -> CMonad InstructionPrepend
translateCondition ELitTrue labelTrue _ =
    return $ instructionListAdd (JUMP JMP $ JumpLabel labelTrue)

translateCondition ELitFalse _ labelFalse =
    return $ instructionListAdd (JUMP JMP $ JumpLabel labelFalse)

translateCondition (Not expression) labelTrue labelFalse =
    translateCondition expression labelFalse labelTrue

translateCondition (ERel expressionL operator expressionR) labelTrue labelFalse = do
    expressionType <- getExpressionType expressionL
    case expressionType of
        Str -> do
            let comparisonExpression = EApp (Ident "compareString") [expressionL, expressionR]
            comparisonCallCode <- translateExpression comparisonExpression
            return $
                comparisonCallCode .
                instructionListMerge [
                    BINARY_INSTRUCTION CMP (LiteralOp 0) (RegisterOp EAX),
                    JUMP (translateOperator operator) $ JumpLabel labelTrue,
                    JUMP JMP $ JumpLabel labelFalse
                ]
        _ -> do
            generatedCodeL <- translateExpression expressionL
            generatedCodeR <- translateExpression expressionR
            return $
                generatedCodeL .
                instructionListAdd (PUSH $ RegisterOp EAX) .
                generatedCodeR .
                instructionListMerge [
                    MOV (RegisterOp EAX) (RegisterOp ECX),
                    POP $ RegisterOp EAX,
                    BINARY_INSTRUCTION CMP (RegisterOp ECX) (RegisterOp EAX),
                    JUMP (translateOperator operator) $ JumpLabel labelTrue,
                    JUMP JMP $ JumpLabel labelFalse
                ]


translateCondition (EAnd expressionL expressionR) labelTrue labelFalse = do
    labelBetween <- getFreeLabel
    beforeLabelBetweenCode <- translateCondition expressionL labelBetween labelFalse
    afterLabelBetweenCode  <- translateCondition expressionR labelTrue labelFalse
    return $
        beforeLabelBetweenCode .
        instructionListAdd (LABEL $ JumpLabel labelBetween) .
        afterLabelBetweenCode

translateCondition (EOr expressionL expressionR) labelTrue labelFalse = do
    labelBetween <- getFreeLabel
    beforeLabelBetweenCode <- translateCondition expressionL labelTrue labelBetween
    afterLabelBetweenCode  <- translateCondition expressionR labelTrue labelFalse
    return $
        beforeLabelBetweenCode .
        instructionListAdd (LABEL $ JumpLabel labelBetween) .
        afterLabelBetweenCode

translateCondition expression labelTrue labelFalse = do
    expressionCode <- translateExpression expression
    return $
        expressionCode .
        instructionListMerge [
            BINARY_INSTRUCTION CMP trueLiteral (RegisterOp EAX),
            JUMP JE $ JumpLabel labelTrue,
            JUMP JMP $ JumpLabel labelFalse
        ]

-----------------------------------
-- get expression type functions --
-----------------------------------
getExpressionType :: Expr -> CMonad Type

getExpressionType (EVar (Ident identifier)) = snd <$> getVariableMapValue identifier

getExpressionType (EArr expressionL _) = do
    (Arr arrayType) <- getExpressionType expressionL
    return arrayType

getExpressionType (EAttr expression (Ident "length")) = do
    attributeType <- getExpressionType expression
    case attributeType of
        (Arr arrayType) -> return arrayType
        _               -> snd <$> getVMTableAttributeMapValue expression "length"

getExpressionType (EAttr expression (Ident identifier)) =
    snd <$> getVMTableAttributeMapValue expression identifier

getExpressionType (EApp (Ident identifier) _) = getFunctionReturnTypeMapValue identifier

getExpressionType (EMeth expression (Ident identifier) expressionList) = do
    (Fun functionType _, _, _) <- getVMTableMethodListValue expression identifier
    return functionType

getExpressionType (ENew arrayType (EArrLen _)) = return $ Arr arrayType

getExpressionType (ENew classType EClsLen) = return classType

getExpressionType (ENullCast (Arr arrayType)) = return (Arr arrayType)

getExpressionType (ENullCast cls@(Cls _)) = return cls

getExpressionType (ELitInt _) = return Int

getExpressionType (ELitTrue) = return Bool

getExpressionType (ELitFalse) = return Bool

getExpressionType (EString _) = return Str

getExpressionType (Neg _) = return Int

getExpressionType (Not _) = return Bool

getExpressionType (EMul _ _ _) = return Int

getExpressionType (EAdd expressionL Plus _) = getExpressionType expressionL

getExpressionType (EAdd _ Minus _) = return Int

getExpressionType (ERel _ _ _) = return Bool

getExpressionType (EAnd _ _) = return Bool

getExpressionType (EOr _ _) = return Bool

-----------------
-- expressions --
-----------------
translateExpression :: Expr -> CMonad InstructionPrepend
translateExpression (EVar (Ident identifier)) = do
    translateExpressionEVar identifier attributeCode localCode
    where
        attributeCode :: Memory -> [Instruction]
        attributeCode loc = [
            MOV (MemoryOp $ Parameter 1) (RegisterOp EAX),
            MOV (MemoryOp loc) (RegisterOp EAX)
            ]

        localCode :: Memory -> [Instruction]
        localCode loc = [
            MOV (MemoryOp loc) (RegisterOp EAX)
            ]

translateExpression eArr@(EArr _ _) = do
    generatedCode <- translateLValue eArr
    return $
        generatedCode .
        instructionListAdd (MOV (AddressOp 0 EAX) (RegisterOp EAX))

translateExpression eAttr@(EAttr expression (Ident identifier)) = do
    attributeType <- getExpressionType expression
    case attributeType of
        (Arr _) -> do
            translateExpression (EArr expression (ELitInt (-1)))
        _ -> do
            generatedCode <- translateLValue eAttr
            return $
                generatedCode .
                instructionListAdd (MOV (AddressOp 0 EAX) (RegisterOp EAX))

translateExpression eApp@(EApp _ _) = translateLValue eApp

translateExpression eMeth@(EMeth _ _ _) = translateLValue eMeth

translateExpression eNew@(ENew _ (EArrLen _)) = translateLValue eNew

translateExpression eNew@(ENew (Cls _) EClsLen) = translateLValue eNew

translateExpression (ENullCast _) = return $ instructionListAdd (MOV nullPointer (RegisterOp EAX))

translateExpression (ELitInt value) = return $ instructionListAdd (MOV (LiteralOp value) (RegisterOp EAX))

translateExpression ELitTrue = return $ instructionListAdd (MOV trueLiteral (RegisterOp EAX))

translateExpression ELitFalse = return $ instructionListAdd (MOV falseLiteral (RegisterOp EAX))

translateExpression (EString string) = do
    label <- getStringLabel string
    return $ instructionListAdd (MOV (StringLiteralOp label) (RegisterOp EAX))

translateExpression (Neg expression) = do
    generatedCode <- translateExpression expression
    return $
        generatedCode .
        instructionListAdd (UNARY_INSTRUCTION NEG (RegisterOp EAX))

translateExpression (Not expression) = do
    generatedCode <- translateExpression expression
    return $
        generatedCode .
        instructionListAdd (BINARY_INSTRUCTION XOR (LiteralOp 1) (RegisterOp EAX))

translateExpression (EMul expressionL Times expressionR) =
    translateBinaryOperands
        expressionL
        expressionR $
        translateBinaryOperator [BINARY_INSTRUCTION MUL (RegisterOp EAX) (RegisterOp ECX)] id

translateExpression (EMul expressionL operator expressionR) = do
    let returnCode = case operator of
            Div -> id
            Mod -> instructionListAdd (MOV (RegisterOp EDX) (RegisterOp EAX))
    generatedCodeL <- translateExpression expressionL
    generatedCodeR <- translateExpression expressionR
    return $
        generatedCodeL .
        instructionListAdd (PUSH $ RegisterOp EAX) .
        generatedCodeR .
        instructionListMerge [
            MOV (RegisterOp EAX) (RegisterOp ECX),
            POP (RegisterOp EAX),
            ZERO_INSTRUCTION CDQ,
            BINARY_INSTRUCTION DIV (RegisterOp ECX) (RegisterOp EAX)
        ] .
        returnCode

translateExpression (EAdd expressionL operator expressionR) = do
    expressionTypeL <- getExpressionType expressionL
    case expressionTypeL of
        Str -> translateExpression (EApp (Ident "concatString") [expressionL, expressionR])
        _   -> do
            let operatorKind = case operator of
                    Plus  -> ADD
                    Minus -> SUB
            translateBinaryOperands
                expressionL
                expressionR $
                translateBinaryOperator [BINARY_INSTRUCTION operatorKind (RegisterOp EAX) (RegisterOp ECX)] id

translateExpression eRel@(ERel expressionL operator expressionR) = do
    expressionTypeL <- getExpressionType expressionL
    case expressionTypeL of
        Str -> do
            let comparisonExpression = EApp (Ident "compareString") [expressionL, expressionR]
            comparisonCallCode <- translateExpression comparisonExpression
            labelTrue <- getFreeLabel
            labelNext <- getFreeLabel
            return $
                comparisonCallCode .
                instructionListMerge [
                    BINARY_INSTRUCTION CMP (LiteralOp 0) (RegisterOp EAX),
                    JUMP (translateOperator operator) $ JumpLabel labelTrue,
                    MOV falseLiteral (RegisterOp EAX),
                    JUMP JMP $ JumpLabel labelNext,
                    LABEL $ JumpLabel labelTrue,
                    MOV trueLiteral (RegisterOp EAX),
                    LABEL $ JumpLabel labelNext
                ]
        _ -> do
            labelTrue  <- getFreeLabel
            labelFalse <- getFreeLabel
            labelNext  <- getFreeLabel
            generatedCode <- translateCondition eRel labelTrue labelFalse
            return $
                generatedCode .
                instructionListMerge [
                    LABEL $ JumpLabel labelFalse,
                    MOV falseLiteral (RegisterOp EAX),
                    JUMP JMP $ JumpLabel labelNext,
                    LABEL $ JumpLabel labelTrue,
                    MOV trueLiteral (RegisterOp EAX),
                    LABEL $ JumpLabel labelNext
                ]

translateExpression eAnd@(EAnd expressionL expressionR) = do
    labelTrue  <- getFreeLabel
    labelFalse <- getFreeLabel
    labelNext  <- getFreeLabel
    generatedCode <- translateCondition eAnd labelTrue labelFalse
    return $
        generatedCode .
        instructionListMerge [
            LABEL $ JumpLabel labelTrue,
            MOV trueLiteral (RegisterOp EAX),
            JUMP JMP $ JumpLabel labelNext,
            LABEL $ JumpLabel labelFalse,
            MOV falseLiteral (RegisterOp EAX),
            LABEL $ JumpLabel labelNext
        ]

translateExpression eOr@(EOr expressionL expressionR) = do
    labelTrue  <- getFreeLabel
    labelFalse <- getFreeLabel
    labelNext  <- getFreeLabel
    generatedCode <- translateCondition eOr labelTrue labelFalse
    return $
        generatedCode .
        instructionListMerge [
            LABEL $ JumpLabel labelFalse,
            MOV falseLiteral (RegisterOp EAX),
            JUMP JMP $ JumpLabel labelNext,
            LABEL $ JumpLabel labelTrue,
            MOV trueLiteral (RegisterOp EAX),
            LABEL $ JumpLabel labelNext
        ]
