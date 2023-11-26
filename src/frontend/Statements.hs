module Statements (statementCheck) where

import Data.Map as Map

import Control.Monad.Reader

import Latte.Abs
import Latte.Print

import Utils
import Expressions


----------------------
-- helper functions --
----------------------
conditionalCheck :: Expr -> [Stmt] -> TCM TCEnvironment
conditionalCheck expression [statement] =
    matchExpressionType TBool expression >> statementCheck statement >> ask

conditionalCheck expression [statementTrue, statementFalse] =
    matchExpressionType TBool expression >> statementCheck statementTrue >> statementCheck statementFalse >> ask

conditionalCheck _ _ = throwTCMonad "Invalid conditional statement"

-------------------------------
-- statement check functions --
-------------------------------
statementCheck :: Stmt -> TCMonad TCEnvironment
statementCheck Empty = ask

statementCheck (BStmt (Block statementList)) = do
    environment  <- ask
    currentScope <- asks scope
    local (\env -> env { scope = currentScope + 1 }) (statementListCheck statementList)
    ask
    where
        statementListCheck :: [Stmt] -> TCMonad TCEnvironment
        statementListCheck statementList = do
            environment <- ask
            foldM foldFunction environment statementList
            where
                foldFunction env statement =
                    local (const env) $ statementCheck statement `throwAdditionalMessage` errorMessage
                    where
                        errorMessage error =
                            error ++
                            "in the following statement:\n" ++
                            printTree statement

statementCheck (Decl type declarationList) = do
    when (type == Void) $ throwTCMonad "Cannot declare variables of type void"
    environment <- ask
    foldM foldFunction environment declarationList
    where
        foldFunction :: TCEnvironment -> Item TCMonad TCEnvironment
        foldFunction accumulator declaration = do
            tcType <- convertTypeToTCType type
            local (const accumulator) $ declarationCheck tcType declaration

        declarationCheck :: TCType -> Item -> TCMonad TCEnvironment
        declarationCheck tcType declaration = do
            variable <- case declaration of
                (NoInit (Ident identifier))            -> return identifier
                (Init   (Ident identifier) expression) ->
                    matchExpressionType tcType expression >> return identifier
            nameAlreadyInScopeCheck variable
            env <- ask
            let environmentAppendDeclared = Map.insert variable (tcType, scope env) (typeMap env)
            return $ env { typeMap = environmentAppendDeclared }

statementCheck (Ass identifier expression) = do
    tcType <- expressionCheck (EVar identifier)
    matchExpressionType tcType expression
    ask

statementCheck (Incr identifier) = expressionCheck (EVar identifier) >>= matchType [TInt] >> ask

statementCheck (Decr identifier) = expressionCheck (EVar identifier) >>= matchType [TInt] >> ask

statementCheck (Ret expression) = matchReturnType =<< expressionCheck expression

statementCheck (VRet) = matchReturnType TVoid

statementCheck (Cond expression statement) = conditionalCheck expression [statement]

statementCheck (CondElse expression statementTrue statementFalse) =
    conditionalCheck expression [statementTrue, statementFalse]

statementCheck (While expression statement) = conditionalCheck expression [statement]

statementCheck (SExp expression) = do
    _ <- expressionCheck expression
    ask
