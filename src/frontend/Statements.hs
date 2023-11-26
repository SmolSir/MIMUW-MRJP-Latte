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
conditionalCheck :: Expr -> [Stmt] -> TCMonad TCEnvironment
conditionalCheck expression [statement] =
    matchExpressionTypeMessage TBool expression >> statementCheck statement >> ask

conditionalCheck expression [statementTrue, statementFalse] =
    matchExpressionTypeMessage TBool expression >> statementCheck statementTrue >> statementCheck statementFalse >> ask

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

statementCheck (Decl declarationType declarationList) = do
    when (declarationType == Void) $ throwTCMonad "Cannot declare variables of type void"
    environment <- ask
    foldM foldFunction environment declarationList
    where
        foldFunction :: TCEnvironment -> Item -> TCMonad TCEnvironment
        foldFunction accumulator declaration = do
            tcType <- convertTypeToTCType declarationType
            local (const accumulator) $ declarationCheck tcType declaration

        declarationCheck :: TCType -> Item -> TCMonad TCEnvironment
        declarationCheck tcType declaration = do
            variable <- case declaration of
                (NoInit (Ident identifier))            -> return identifier
                (Init   (Ident identifier) expression) ->
                    matchExpressionTypeMessage tcType expression >> return identifier
            nameAlreadyInScopeCheck variable
            env <- ask
            let environmentAppendDeclared = Map.insert variable (tcType, scope env) (typeMap env)
            return $ env { typeMap = environmentAppendDeclared }

statementCheck (Ass identifier expression) = do
    tcType <- statementExpressionCheck (EVar identifier)
    matchExpressionTypeMessage tcType expression
    ask

statementCheck (Incr identifier) = statementExpressionCheck (EVar identifier) >>= matchType [TInt] >> ask

statementCheck (Decr identifier) = statementExpressionCheck (EVar identifier) >>= matchType [TInt] >> ask

statementCheck (Ret expression) = matchReturnType =<< statementExpressionCheck expression

statementCheck (VRet) = matchReturnType TVoid

statementCheck (Cond expression statement) = conditionalCheck expression [statement]

statementCheck (CondElse expression statementTrue statementFalse) =
    conditionalCheck expression [statementTrue, statementFalse]

statementCheck (While expression statement) = conditionalCheck expression [statement]

statementCheck (SExp expression) = do
    _ <- statementExpressionCheck expression
    ask
