module Expressions (
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

import Utils


----------------------------
-- helper match functions --
----------------------------
matchExpressionType :: TCType -> Expr -> TCMonad TCType
matchExpressionType expectedType expression = do
    actualType <- expressionCheck expression
    matchType [expectedType] actualType
    return actualType

matchExpressionTypeMessage :: TCType -> Expr -> TCMonad TCType
matchExpressionTypeMessage expectedType expression =
    matchExpressionType expectedType expression `throwAdditionalMessage` errorMessage
    where
        errorMessage error = error ++ " in the following expression:\n" ++ printTree expression

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

--------------------------------
-- expression check functions --
--------------------------------
expressionCheck :: Expr -> TCMonad TCType
expressionCheck (EVar (Ident variable)) = variableType variable

expressionCheck (ELitInt _) = return TInt

expressionCheck (ELitTrue) = return TBool

expressionCheck (ELitFalse) = return TBool

expressionCheck (EApp (Ident identifier) expressionList) = do
    typeScope <- variableType identifier
    case typeScope of
        TFun argumentList returnType -> do
            argumentListCheck argumentList expressionList
            return returnType
        _ -> do
            throwTCMonad (identifier ++ " is not a function")

expressionCheck (EString _) = return TString

expressionCheck (Neg expression) = matchExpressionType TInt expression

expressionCheck (Not expression) = matchExpressionType TBool expression

expressionCheck (EMul expressionL _ expressionR) = operatorCheck [TInt] expressionL expressionR

expressionCheck (EAdd expressionL Plus expressionR) = operatorCheck [TInt, TString] expressionL expressionR

expressionCheck (EAdd expressionL Minus expressionR) = operatorCheck [TInt] expressionL expressionR

expressionCheck (ERel expressionL EQU expressionR) =
    operatorCheck [TInt, TString, TBool] expressionL expressionR >> return TBool

expressionCheck (ERel expressionL NE expressionR) =
    operatorCheck [TInt, TString, TBool] expressionL expressionR >> return TBool

expressionCheck (ERel expressionL _ expressionR) =
    operatorCheck [TInt, TString] expressionL expressionR >> return TBool

expressionCheck (EAnd expressionL expressionR) = operatorCheck [TBool] expressionL expressionR

expressionCheck (EOr expressionL expressionR) = operatorCheck [TBool] expressionL expressionR
