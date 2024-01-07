module Backend (runCompiler) where

import Data.List as List
import Data.Map as Map
import Data.Bits as Bits

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

import Latte.Abs

-----------------------------------
-- compiler runtime types & data --
-----------------------------------
type Offset = Integer
type Scope = Integer

type Variable = String
type VariableMap = Map.Map Variable (Memory, Type)

data CEnvironment = CEnvironment {
    scope :: Scope,
    varaibleMap :: VariableMap
}

data CState = CState {
    localsCount :: Integer,
    stackSize :: Integer,
    stackSizeMax :: Integer,
    nextLabel :: Integer,
    returnLabel :: String,
    stringMap :: Map.Map String Label,
    functionReturnTypeMap :: Map.Map Variable Type

}

type CMonad a = ReaderT CEnvironment (StateT CState (ExceptT String IO)) a

----------------------------------
-- code generation types & data --
----------------------------------
data Register = EAX | ECX | EDX | EBP | ESP

data Memory = Local Integer | Parameter Integer | Stack Integer

data Operand = RegisterOp Register | MemoryOp Memory | LiteralOp Integer | StringLiteralOp Integer

data BinaryOperator = ADD | SUB | MUL | DIV | CMP | XOR

data UnaryOperator = INC | DEC | NEG

data ZeroOperator = RET | CDQ -- CDQ -> Convert Doubleword to Quadword
    deriving Show

data JumpOperator = JL | JLE | JE | JGE | JG | JNE | JMP
    deriving Show

data Label = FunctionLabel String | JumpLabel Integer | StringLabel Integer String

data Instruction =
      ENTRY
    | HEADER
    | FOOTER
    | STACK_ALLOCATION Integer
    | PUSH Operand
    | POP Operand
    | MOV Operand Operand
    | CALL String Integer
    | BINARY_INSTRUCTION BinaryOperator Operand Operand
    | UNARY_INSTRUCTION UnaryOperator Operand
    | ZERO_INSTRUCTION ZeroOperator
    | JUMP JumpOperator Label
    | LABEL Label

-- constants --
dword = 4
trueLiteral = LiteralOp 1
falseLiteral = LiteralOp 0

------------------------------------
-- code generation show functions --
------------------------------------
instance Show Register where
    show EAX = "%eax"
    show ECX = "%ecx"
    show EDX = "%edx"
    show EBP = "%ebp"
    show ESP = "%esp"

instance Show Memory where
    show (Local value)     = show (-dword * value) ++ "(" ++ show EBP ++ ")"
    show (Parameter value) = show (dword * (value + 1)) ++ "(" ++ show EBP ++ ")"
    show (Stack value)     = show (-dword * value) ++ "(" ++ show EBP ++ ")"

instance Show Operand where
    show (RegisterOp register) = show register
    show (MemoryOp memory) = show memory
    show (LiteralOp literal) = '$' : show literal
    show (StringLiteralOp stringLiteral) = '$' : show (StringLabel stringLiteral "")

instance Show BinaryOperator where
    show ADD = "addl" ++ " "
    show SUB = "subl" ++ " "
    show MUL = "imul" ++ " "
    show DIV = "idiv" ++ " "
    show CMP = "cmp" ++ " "
    show XOR = "xor" ++ " "

instance Show UnaryOperator where
    show INC = "incl" ++ " "
    show DEC = "decl" ++ " "
    show NEG = "neg" ++ " "

instance Show Label where
    show (FunctionLabel function  ) = function
    show (JumpLabel     jump      ) = ".L"  ++ show jump
    show (StringLabel   string   _) = ".LC" ++ show string

instance Show Instruction where
    show ENTRY = "\
        \.text\n\
        \.globl main\n\
        \"

    show HEADER =
        show (PUSH $ RegisterOp EBP) ++ "\n" ++
        show (MOV (RegisterOp ESP) $ RegisterOp EBP)

    show FOOTER =
        show (MOV (RegisterOp EBP) $ RegisterOp ESP) ++ "\n" ++
        show (POP $ RegisterOp EBP)

    show (STACK_ALLOCATION value) =
        show $ BINARY_INSTRUCTION SUB (LiteralOp (dword * value)) $ RegisterOp ESP

    show (PUSH operand) = "pushl" ++ " " ++ show operand
    show (POP  operand) = "popl"  ++ " " ++ show operand

    show (MOV operandL operandR) = "movl" ++ " " ++ show operandL ++ ", " ++ show operandR

    show (CALL literal _) = "call" ++ " " ++ literal

    show (BINARY_INSTRUCTION binaryOperator operandL operandR) =
        show binaryOperator ++ " " ++ show operandL ++ ", " ++ show operandR

    show (UNARY_INSTRUCTION unaryOperator operand) = show unaryOperator ++ " " ++ show operand

    show (ZERO_INSTRUCTION zeroOperator) = show zeroOperator

    show (JUMP jumpOperator label) = show jumpOperator ++ " " ++ show label

    show (LABEL label@(StringLabel _ stringLabel)) =
        show label ++ ":" ++ "\n" ++
        "    " ++ ".string" ++ " " ++ show stringLabel

    show (LABEL label) = show label ++ ":"

----------------------
-- helper functions --
----------------------
type InstructionPrepend = [Instruction] -> [Instruction]

-- prepends an instruction to a list of instructions
instructionListAdd :: Instruction -> InstructionPrepend
instructionListAdd = (:)

-- merges two lists of instructions
instructionListMerge :: [Instruction] -> InstructionPrepend
instructionListMerge = (++)

instructionListUnpack :: [Instruction] -> CMonad String
instructionListUnpack instructionList = return $ (unlines . List.map show) instructionList

getFreeLabel :: CMonad Integer
getFreeLabel = do
    label <- gets nextLabel
    modify (\state -> state { nextLabel = label + 1 })
    return label

getStringLabel :: String -> CMonad Integer
getStringLabel string = do
    strMap <- gets stringMap
    case Map.lookup string strMap of
        Nothing -> do
            let label = fromIntegral $ Map.size strMap
            modify (\state -> state {
                stringMap = Map.insert string (StringLabel label string) strMap
            })
            return label
        Just (StringLabel label string) -> return label

throwCMonad :: String -> CMonad a
throwCMonad = lift . lift . throwE

-----------------
-- expressions --
-----------------
getExpressionType :: Expr -> CMonad Type

getExpressionType (EVar (Ident identifier)) = do
    environment <- ask
    case Map.lookup identifier (varaibleMap environment) of
        Nothing                  -> throwCMonad "Not possible to get type of expression of kind `EVar Ident`"
        Just (_, expressionType) -> return expressionType

getExpressionType (ELitInt _) = return Int

getExpressionType (ELitTrue) = return Bool

getExpressionType (ELitFalse) = return Bool

getExpressionType (EApp (Ident identifier) _) = do
    state <- get
    case Map.lookup identifier (functionReturnTypeMap state) of
        Nothing             -> throwCMonad "Not possible to get type of expression of kind `EApp Ident [Expr]`"
        Just expressionType -> return expressionType

getExpressionType (EString _) = return Str

getExpressionType (Neg _) = return Int

getExpressionType (Not _) = return Bool

getExpressionType (EMul _ _ _) = return Int

getExpressionType (EAdd expression Plus _) = getExpressionType expression

getExpressionType (EAdd expression Minus _) = return Int

getExpressionType (ERel _ _ _) = return Bool

getExpressionType (EAnd _ _) = return Bool

getExpressionType (EOr _ _) = return Bool


translateExpression :: Expr -> CMonad InstructionPrepend

translateExpression (EVar (Ident identifier)) = do
    environment <- ask
    case Map.lookup identifier (varaibleMap environment) of
        Nothing            -> throwCMonad "Not possible to translate expression of kind `EVar Ident`"
        Just (location, _) -> return $ instructionListAdd $ PUSH $ MemoryOp location

translateExpression (ELitInt value) = return $ instructionListAdd $ PUSH (LiteralOp value)

translateExpression ELitTrue = return (PUSH trueLiteral :)

translateExpression ELitFalse = return (PUSH falseLiteral :)

translateExpression (EApp (Ident identifier) expressionList) = do
    reverseTranslatedExpressionList <- mapM translateExpression expressionList
    let translatedExpressionList = List.foldr (.) id (reverse reverseTranslatedExpressionList)
    return $ translatedExpressionList . instructionListMerge [
        CALL identifier $ fromIntegral $ length expressionList,
        BINARY_INSTRUCTION ADD (LiteralOp (dword * fromIntegral (length expressionList))) $ RegisterOp ESP,
        PUSH $ RegisterOp EAX
        ]

translateExpression (EString string) = do
    label <- getStringLabel string
    return $ instructionListAdd $ PUSH $ StringLiteralOp label

translateExpression (Neg expression) = do
    generatedCode <- translateExpression expression
    return $ generatedCode . neg
    where
        neg :: InstructionPrepend
        neg = instructionListMerge [
            POP $ RegisterOp EAX,
            UNARY_INSTRUCTION NEG $ RegisterOp EAX,
            PUSH $ RegisterOp EAX
            ]

translateExpression (Not expression) = do
    generatedCode <- translateExpression expression
    return $ generatedCode . not
    where
        not :: InstructionPrepend
        not = instructionListMerge [
            POP $ RegisterOp EAX,
            BINARY_INSTRUCTION XOR (LiteralOp 1) $ RegisterOp EAX,
            PUSH $ RegisterOp EAX
            ]

translateExpression (EMul expressionL Times expressionR) =
    translateBinaryOperands expressionL expressionR $ translateBinaryOperator [BINARY_INSTRUCTION MUL (RegisterOp ECX) $ RegisterOp EAX] $ RegisterOp EAX

translateExpression (EMul expressionL Div expressionR) =
    translateBinaryOperands expressionL expressionR $ translateBinaryOperator [ZERO_INSTRUCTION CDQ, BINARY_INSTRUCTION DIV (RegisterOp ECX) $ RegisterOp EAX] $ RegisterOp EAX

translateExpression (EMul expressionL Mod expressionR) =
    translateBinaryOperands expressionL expressionR $ translateBinaryOperator [ZERO_INSTRUCTION CDQ, BINARY_INSTRUCTION DIV (RegisterOp ECX) $ RegisterOp EAX] $ RegisterOp EDX

translateExpression (EAdd expressionL operator expressionR) = do
    expressionTypeL <- getExpressionType expressionL
    case expressionTypeL of
        Str -> translateExpression (EApp (Ident "concatString") [expressionL, expressionR])
        _   -> do
            let operatorKind = case operator of
                    Plus  -> ADD
                    Minus -> SUB
            translateBinaryOperands expressionL expressionR $ translateBinaryOperator [BINARY_INSTRUCTION operatorKind (RegisterOp ECX) $ RegisterOp EAX] $ RegisterOp EAX

translateExpression (ERel expressionL operator expressionR) = do
    generatedCodeL <- translateExpression expressionL
    generatedCodeR <- translateExpression expressionR
    labelTrue <- getFreeLabel
    labelNext <- getFreeLabel
    let instructionList = [
            POP $ RegisterOp ECX,
            POP $ RegisterOp EAX,
            BINARY_INSTRUCTION CMP (RegisterOp ECX) $ RegisterOp EAX,
            JUMP (translateOperator operator) $ JumpLabel labelTrue,
            PUSH falseLiteral,
            JUMP JMP $ JumpLabel labelNext,
            LABEL $ JumpLabel labelTrue,
            PUSH trueLiteral,
            LABEL $ JumpLabel labelNext
            ]
    return $ generatedCodeL . generatedCodeR . instructionListMerge instructionList
    where
        translateOperator :: RelOp -> JumpOperator
        translateOperator operator = case operator of
            LTH -> JL
            LE  -> JLE
            EQU -> JE
            GE  -> JGE
            GTH -> JG
            NE  -> JNE

translateExpression (EAnd expressionL expressionR) = do
    generatedCodeL <- translateExpression expressionL
    generatedCodeR <- translateExpression expressionR
    labelFalse <- getFreeLabel
    labelNext  <- getFreeLabel
    let catchFalse = instructionListMerge [
            POP $ RegisterOp EAX,
            BINARY_INSTRUCTION CMP falseLiteral $ RegisterOp EAX,
            JUMP JE $ JumpLabel labelFalse
            ]
    let allTrue = instructionListMerge [
            PUSH trueLiteral,
            JUMP JMP $ JumpLabel labelNext
            ]
    let resultCollector = instructionListMerge [
            LABEL $ JumpLabel labelFalse,
            PUSH falseLiteral,
            LABEL $ JumpLabel labelNext
            ]
    return $ generatedCodeL . catchFalse . generatedCodeR . catchFalse . allTrue . resultCollector

translateExpression (EOr expressionL expressionR) = do
    generatedCodeL <- translateExpression expressionL
    generatedCodeR <- translateExpression expressionR
    labelTrue <- getFreeLabel
    labelNext <- getFreeLabel
    let catchTrue = instructionListMerge [
            POP $ RegisterOp EAX,
            BINARY_INSTRUCTION CMP trueLiteral $ RegisterOp EAX,
            JUMP JE $ JumpLabel labelTrue
            ]
    let allFalse = instructionListMerge [
            PUSH falseLiteral,
            JUMP JMP $ JumpLabel labelNext
            ]
    let resultCollector = instructionListMerge [
            LABEL $ JumpLabel labelTrue,
            PUSH trueLiteral,
            LABEL $ JumpLabel labelNext
            ]
    return $ generatedCodeL . catchTrue . generatedCodeR . catchTrue . allFalse . resultCollector

----------------------------------
-- statements' helper functions --
----------------------------------
translateIdentifier :: Ident -> CMonad (Memory, InstructionPrepend)
translateIdentifier (Ident identifier) = do
    environment <- ask
    case Map.lookup identifier (varaibleMap environment) of
        Nothing            -> throwCMonad "Not possible to translate assignment expression of kind `EVar Ident`"
        Just (location, _) -> return (location, id)

translateDeclaration :: Type -> Item -> CMonad (CEnvironment, InstructionPrepend)
translateDeclaration declarationType declaration = do
    state <- get
    scope <- asks scope
    let location = Local $ localsCount state + 1
    modify (\state -> state {
        localsCount = localsCount state + 1
    })
    (identifier, generatedCode) <- case declaration of
        (NoInit (Ident identifier)) -> do
            value <- defaultValueForType declarationType
            return (
                identifier,
                instructionListMerge [
                    MOV value (MemoryOp location)
                ])
        (Init (Ident identifier) expression) -> do
            initializationCode <- translateExpression expression
            return (
                identifier,
                initializationCode . instructionListMerge [
                    POP $ RegisterOp EAX,
                    MOV (RegisterOp EAX) (MemoryOp location)
                ])
    env <- ask
    let updatedEnvironment = Map.insert identifier (location, declarationType) $ varaibleMap env
    return (
        env { varaibleMap = updatedEnvironment }, generatedCode
        )
    where
        defaultValueForType :: Type -> CMonad Operand
        defaultValueForType Int  = return $ LiteralOp 0
        defaultValueForType Bool = return falseLiteral
        defaultValueForType Str  = StringLiteralOp <$> getStringLabel ""

translateStatementList :: [Stmt] -> CMonad (CEnvironment, InstructionPrepend)
translateStatementList statementList = do
    environment        <- ask
    (_, generatedCode) <- foldM go (environment, id) statementList
    return (environment, generatedCode)
    where
        go :: (CEnvironment, InstructionPrepend) -> Stmt -> CMonad (CEnvironment, InstructionPrepend)
        go (currentEnvironment, accumulatedCode) statement = do
            (resultEnvironment, resultCode) <- local (const currentEnvironment) $ translateStatement statement
            return (resultEnvironment, accumulatedCode . resultCode)

translateStatementBlockWrap :: Stmt -> CMonad InstructionPrepend
translateStatementBlockWrap statement@(BStmt _) = do
    (_, generatedCode) <- translateStatement statement
    return generatedCode

translateStatementBlockWrap statement = do
    (_, generatedCode) <- translateStatement (BStmt (Block [statement]))
    return generatedCode

----------------
-- statements --
----------------
translateStatement :: Stmt -> CMonad (CEnvironment, InstructionPrepend)
translateStatement Empty = do
    environment <- ask
    return (
        environment,
        id
        )

translateStatement (BStmt (Block statementList)) = do
    environment <- ask
    local (\environment -> environment { scope = scope environment + 1 }) $ translateStatementList statementList

translateStatement (Decl declarationType declarationList) = do
    environment <- ask
    foldM (go declarationType) (environment, id) declarationList
    where
        go :: Type -> (CEnvironment, InstructionPrepend) -> Item -> CMonad (CEnvironment, InstructionPrepend)
        go declType (currentEnvironment, accumulatedCode) declaration = do
            (resultEnvironment, resultCode) <- local (const currentEnvironment) $ translateDeclaration declType declaration
            return (
                resultEnvironment,
                accumulatedCode . resultCode
                )

translateStatement (Ass identifier expression) = do
    expressionCode <- translateExpression expression
    (memory, _) <- translateIdentifier identifier
    environment <- ask
    return (
        environment,
        expressionCode . instructionListMerge [
            POP $ RegisterOp EAX,
            MOV (RegisterOp EAX) $ MemoryOp memory
        ])

translateStatement (Incr identifier) = do
    (memory, _) <- translateIdentifier identifier
    environment <- ask
    return (
        environment,
        instructionListMerge [
            UNARY_INSTRUCTION INC $ MemoryOp memory
        ])

translateStatement (Decr identifier) = do
    (memory, _) <- translateIdentifier identifier
    environment <- ask
    return (
        environment,
        instructionListMerge [
            UNARY_INSTRUCTION DEC $ MemoryOp memory
        ])

translateStatement (Ret expression) = do
    generatedCode <- translateExpression expression
    environment <- ask
    label <- gets returnLabel
    return (
        environment,
        generatedCode . instructionListMerge [
            POP $ RegisterOp EAX,
            JUMP JMP $ FunctionLabel label
        ])

translateStatement VRet = do
    environment <- ask
    label <- gets returnLabel
    return (
        environment,
        instructionListAdd $ JUMP JMP $ FunctionLabel label
        )

translateStatement (Cond expression statement) = do
    expressionConditionCode <- translateExpression expression
    statementTrueCode <- translateStatementBlockWrap statement
    labelNext <- getFreeLabel
    let catchFalse = instructionListMerge [
            POP $ RegisterOp EAX,
            BINARY_INSTRUCTION CMP falseLiteral $ RegisterOp EAX,
            JUMP JE $ JumpLabel labelNext
            ]
    let resultCollector = instructionListMerge [
            LABEL $ JumpLabel labelNext
            ]
    environment <- ask
    return (
        environment,
        expressionConditionCode . catchFalse . statementTrueCode . resultCollector
        )

translateStatement (CondElse expression statementL statementR) = do
    expressionConditionCode <- translateExpression expression
    statementTrueCode       <- translateStatementBlockWrap statementL
    statementFalseCode      <- translateStatementBlockWrap statementR
    labelFalse <- getFreeLabel
    labelNext  <- getFreeLabel
    let catchFalse = instructionListMerge [
            POP $ RegisterOp EAX,
            BINARY_INSTRUCTION CMP falseLiteral $ RegisterOp EAX,
            JUMP JE $ JumpLabel labelFalse
            ]
    let afterTrue = instructionListMerge [
            JUMP JMP $ JumpLabel labelNext
            ]
    let resultCollectorFalse = instructionListMerge [
            LABEL $ JumpLabel labelFalse
            ]
    let resultCollectorTrue = instructionListMerge [
            LABEL $ JumpLabel labelNext
            ]
    environment <- ask
    return (
        environment,
        expressionConditionCode . catchFalse . statementTrueCode . afterTrue . resultCollectorFalse . statementFalseCode . resultCollectorTrue
        )

translateStatement (While expression statement) = do
    expressionConditionCode <- translateExpression expression
    statementLoopCode       <- translateStatementBlockWrap statement
    labelCondition <- getFreeLabel
    labelLoop      <- getFreeLabel
    let entryCode = instructionListMerge [
            JUMP JMP $ JumpLabel labelCondition
            ]
    let loopLabelCode = instructionListMerge [
            LABEL $ JumpLabel labelLoop
            ]
    let conditionLabelCode = instructionListMerge [
            LABEL $ JumpLabel labelCondition
            ]
    let conditionCheckCode = instructionListMerge [
            POP $ RegisterOp EAX,
            BINARY_INSTRUCTION CMP trueLiteral $ RegisterOp EAX,
            JUMP JE $ JumpLabel labelLoop
            ]
    environment <- ask
    return (
        environment,
        entryCode . loopLabelCode . statementLoopCode . conditionLabelCode . expressionConditionCode . conditionCheckCode
        )

translateStatement (SExp expression) = do
    generatedCode <- translateExpression expression
    environment   <- ask
    return (
        environment,
        generatedCode
        )

------------
-- others --
------------
translateTopDef :: TopDef -> CMonad InstructionPrepend
translateTopDef topDef = case topDef of
    FnDef returnType (Ident identifier) argumentList block -> do
        modify (\state -> state {
            localsCount           = 0,
            returnLabel           = "ret_" ++ identifier,
            functionReturnTypeMap = Map.insert identifier returnType $ functionReturnTypeMap state
        })

        (_, code) <- local
            (\environment -> environment {
                varaibleMap = Map.fromList $ zipWith
                    (\(Arg argumentType (Ident argumentIdentifier)) argumentValue -> (argumentIdentifier, (Parameter argumentValue, argumentType)))
                    argumentList
                    [1 ..]
            })
            (translateStatement (BStmt block))

        state <- get
        return
            $ instructionListMerge [LABEL $ FunctionLabel identifier, HEADER, STACK_ALLOCATION $ localsCount state]
            . code
            . instructionListMerge [LABEL $ FunctionLabel $ returnLabel state, FOOTER, ZERO_INSTRUCTION RET]

translateBinaryOperator :: [Instruction] -> Operand -> InstructionPrepend
translateBinaryOperator operatorList resultRegisterOperand =
    instructionListMerge $ [POP $ RegisterOp ECX, POP $ RegisterOp EAX] ++ operatorList ++ [PUSH resultRegisterOperand]

translateBinaryOperands :: Expr -> Expr -> InstructionPrepend -> CMonad InstructionPrepend
translateBinaryOperands expressionL expressionR operandCode = do
    codeL <- translateExpression expressionL
    codeR <- translateExpression expressionR
    return $ codeL . codeR . operandCode

-------------------
-- run functions --
-------------------
runCompiler (Program prog) =
    evalStateT (runReaderT (go prog) (CEnvironment 0 Map.empty)) (CState 0 0 0 0 "" Map.empty predefinedFunctionMap)
    where
        go prog = flip ($) [] <$> generateExpressionList prog >>= instructionListUnpack

        predefinedFunctionMap :: Map.Map Variable Type
        predefinedFunctionMap = Map.fromList [
            ("error"        , Void),
            ("readInt"      , Int ),
            ("readString"   , Str ),
            ("printInt"     , Void),
            ("printString"  , Void),
            ("concatString" , Str ),
            ("compareString", Int )
            ]

generateExpressionList :: [TopDef] -> CMonad InstructionPrepend
generateExpressionList topDefList = do
    generatedCode <- foldM go id topDefList
    strMap        <- gets stringMap
    let stringLabelList = List.map LABEL $ Map.elems strMap
    return $ instructionListAdd ENTRY . instructionListMerge stringLabelList . generatedCode
    where
        go accumulatedCode statement = do
            generatedCode <- translateTopDef statement
            return (accumulatedCode . generatedCode)
