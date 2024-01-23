module TC_Expressions (
    expressionCheck,
    statementExpressionCheck,
    matchExpressionType,
    matchExpressionTypeMessage
) where

import Data.Map as Map

import Control.Monad
import Control.Monad.Reader

import Latte.Abs
import Latte.Print

import TC_Utils


----------------------------
-- helper match functions --
----------------------------
matchExpressionType :: TCType -> Expr -> TCMonad TCType
matchExpressionType expectedType expression
    | expectedType == metaArray = do
        actualType <- expressionCheck expression
        case actualType of
            (TArr arrayType) -> return actualType
            _                -> throwTCMonad $
                "Expected some array type, but the actual type was: " ++
                show actualType
        return actualType
    | expectedType == metaClass = do
        actualType <- expressionCheck expression
        case actualType of
            (TCls className) -> return actualType
            _                -> throwTCMonad $
                "Expected some class type, but the actual type was: " ++
                show actualType
        return actualType
    | otherwise = do
        actualType <- expressionCheck expression
        matchType [expectedType] actualType
        return actualType

matchExpressionTypeMessage :: TCType -> Expr -> TCMonad TCType
matchExpressionTypeMessage expectedType expression =
    matchExpressionType expectedType expression `throwAdditionalMessage` errorMessage
    where
        errorMessage error = error ++ " in the following expression:\n" ++ printTree expression

matchAttribute :: Variable -> Ident -> TCMonad TCType
matchAttribute className ident@(Ident identifier) = do
    classMap <- asks classMap
    case Map.lookup className classMap of
        Nothing ->
            throwTCMonad $
                "Impossible error: class `" ++
                className ++
                "` has been found by matching. It must exist"
        (Just classDefinition) ->
            case Map.lookup identifier (memberMap classDefinition) of
                Nothing ->
                    case extends classDefinition of
                        Nothing ->
                            throwTCMonad $
                                "Usage of undeclared attribute `" ++
                                identifier ++
                                "`\n"
                        (Just parentClassName) ->
                            matchAttribute parentClassName ident
                (Just (TFun _ _)) ->
                    throwTCMonad $
                        "Expected attribute: `" ++
                        identifier ++
                        "` but found a method instead"
                (Just attributeType) ->
                    return attributeType

matchMethod :: Variable -> Ident -> [Expr] -> TCMonad TCType
matchMethod className ident@(Ident identifier) expressionList = do
    classMap <- asks classMap
    case Map.lookup className classMap of
        Nothing ->
            throwTCMonad $
                "Impossible error: class `" ++
                className ++
                "` has been found by matching. It must exist"
        (Just classDefinition) ->
            case Map.lookup identifier (memberMap classDefinition) of
                Nothing ->
                    case extends classDefinition of
                        Nothing ->
                            throwTCMonad $
                                "Usage of undeclared method `" ++
                                identifier ++
                                "`\n"
                        (Just parentClassName) ->
                            matchMethod parentClassName ident expressionList
                (Just (TFun argumentList returnType)) -> do
                    argumentListCheck argumentList expressionList
                    return returnType
                (Just attributeType) ->
                    throwTCMonad $
                        "Expected method `" ++
                        identifier ++
                        "` but found an attribute instead"

----------------------------
-- helper check functions --
----------------------------
statementExpressionCheck :: Expr -> TCMonad TCType
statementExpressionCheck expression = expressionCheck expression `throwAdditionalMessage` errorMessage
    where
        errorMessage error = error ++ " in the following expression:\n" ++ printTree expression

argumentListCheck :: [TCType] -> [Expr] -> TCMonad ()
argumentListCheck argumentList expressionList =
    if length argumentList == length expressionList
        then
            mapM_ argumentCheck $ zip argumentList expressionList
        else
            throwTCMonad "Invalid number of arguments for the called function"
        where
            argumentCheck :: (TCType, Expr) -> TCMonad TCType
            argumentCheck (tcType, expression) = matchExpressionType tcType expression

operatorCheck :: [TCType] -> Expr -> Expr -> TCMonad TCType
operatorCheck tcTypeList expressionL expressionR = do
    typeL <- expressionCheck expressionL
    typeR <- expressionCheck expressionR
    matchType tcTypeList typeL
    matchType [typeL] typeR
    return typeL

equalityOperatorCheck :: [TCType] -> Expr -> Expr -> TCMonad TCType
equalityOperatorCheck tcTypeList expressionL expressionR = do
    typeL <- expressionCheck expressionL
    typeR <- expressionCheck expressionR
    case (typeL, typeR) of
        (TArr arrayTypeL, TArr arrayTypeR) -> arrayCompatibilityCheck arrayTypeL arrayTypeR
        (TCls classTypeL, TCls classTypeR) -> classCompatibilityCheck classTypeL classTypeR
        _ -> do
            matchType tcTypeList typeL
            matchType [typeL] typeR
    return typeL
    where
        arrayCompatibilityCheck :: TCType -> TCType -> TCMonad ()
        arrayCompatibilityCheck (TArr arrayTypeL) (TArr arrayTypeR) = arrayCompatibilityCheck arrayTypeL arrayTypeR
        arrayCompatibilityCheck (TCls classTypeL) (TCls classTypeR) = classCompatibilityCheck classTypeL classTypeR
        arrayCompatibilityCheck tcTypeL tcTypeR = matchType [tcTypeL] tcTypeR

        classCompatibilityCheck :: Variable -> Variable -> TCMonad ()
        classCompatibilityCheck classL classR = do
            compatibleClassListL <- getCompatibleClassList classL
            when (notElem classR compatibleClassListL) $ do
                compatibleClassListR <- getCompatibleClassList classR
                when (notElem classL compatibleClassListR) $
                    throwTCMonad $
                        "Cannot compare type: " ++
                        classL ++
                        "with type: " ++
                        classR

expressionAttributeCheck :: Expr -> Ident -> TCMonad TCType
expressionAttributeCheck expression identifier = do
    (TCls className) <- matchExpressionType metaClass expression
    matchAttribute className identifier

--------------------------------
-- expression check functions --
--------------------------------
expressionCheck :: Expr -> TCMonad TCType
expressionCheck (EVar (Ident variable)) = variableType variable

expressionCheck (EArr expressionL expressionR) = do
    (TArr actualType) <- matchExpressionType metaArray expressionL
    matchExpressionType TInt expressionR
    return actualType

expressionCheck (EAttr expression ident@(Ident identifier)) = do
    if identifier == "length"
        then do
            actualType <- matchExpressionType metaArray expression
            case actualType of
                (TArr _) -> return TInt
                _        -> expressionAttributeCheck expression ident
        else do
            expressionAttributeCheck expression ident

expressionCheck (EApp (Ident identifier) expressionList) = do
    typeScope <- variableType identifier
    case typeScope of
        TFun argumentList returnType -> do
            argumentListCheck argumentList expressionList
            return returnType
        _ -> do
            throwTCMonad (identifier ++ " is not a function")

expressionCheck (EMeth expression ident@(Ident identifier) expressionList) = do
    (TCls className) <- matchExpressionType metaClass expression
    matchMethod className ident expressionList

expressionCheck (ENew (Cls (Ident identifier)) EClsLen) =
    classExistsCheck identifier >> return (TCls identifier)

expressionCheck (ENew arrayType (EArrLen expression)) = do
    classTypeExistsCheck arrayType
    matchExpressionType TInt expression
    TArr <$> convertTypeToTCType arrayType

expressionCheck (ENew _ _) =
    throwTCMonad "Invalid `new` expression"

expressionCheck (ENullCast (Arr arrayType)) =
    classTypeExistsCheck arrayType >> TArr <$> convertTypeToTCType arrayType

expressionCheck (ENullCast (Cls ident@(Ident identifier))) =
    classTypeExistsCheck (Cls ident) >> return (TCls identifier)

expressionCheck (ENullCast _) =
    throwTCMonad "Invalid `null` cast expression"

expressionCheck (ELitInt _) = return TInt

expressionCheck (ELitTrue) = return TBool

expressionCheck (ELitFalse) = return TBool

expressionCheck (EString _) = return TString

expressionCheck (Neg expression) = matchExpressionType TInt expression

expressionCheck (Not expression) = matchExpressionType TBool expression

expressionCheck (EMul expressionL _ expressionR) = operatorCheck [TInt] expressionL expressionR

expressionCheck (EAdd expressionL Plus expressionR) = operatorCheck [TInt, TString] expressionL expressionR

expressionCheck (EAdd expressionL Minus expressionR) = operatorCheck [TInt] expressionL expressionR

expressionCheck (ERel expressionL EQU expressionR) =
    equalityOperatorCheck [TInt, TString, TBool, metaArray, metaClass] expressionL expressionR >> return TBool

expressionCheck (ERel expressionL NE expressionR) =
    equalityOperatorCheck [TInt, TString, TBool, metaArray, metaClass] expressionL expressionR >> return TBool

expressionCheck (ERel expressionL _ expressionR) =
    operatorCheck [TInt, TString] expressionL expressionR >> return TBool

expressionCheck (EAnd expressionL expressionR) = operatorCheck [TBool] expressionL expressionR

expressionCheck (EOr expressionL expressionR) = operatorCheck [TBool] expressionL expressionR
