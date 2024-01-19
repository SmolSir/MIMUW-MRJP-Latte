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
    matchExpressionTypeMessage TBool expression >>
    statementBlockWrapCheck statement >>
    ask

conditionalCheck expression [statementTrue, statementFalse] =
    matchExpressionTypeMessage TBool expression >>
    statementBlockWrapCheck statementTrue >>
    statementBlockWrapCheck statementFalse >>
    ask

conditionalCheck _ _ = throwTCMonad "Invalid conditional statement"

statementBlockWrapCheck :: Stmt -> TCMonad TCEnvironment
statementBlockWrapCheck statement@(BStmt _) = statementCheck statement

statementBlockWrapCheck statement = statementCheck (BStmt (Block [statement]))

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
                foldFunction :: TCEnvironment -> Stmt -> TCMonad TCEnvironment
                foldFunction env statement =
                    local (const env) $ statementCheck statement `throwAdditionalMessage` errorMessage
                    where
                        errorMessage error =
                            error ++
                            " in the following statement:\n" ++
                            printTree statement ++
                            "\n"

statementCheck (Decl declarationType declarationList) = do
    when (voidTypeCheck declarationType) $ throwTCMonad "Cannot declare variables of type void"
    classTypeExistsCheck declarationType
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

statementCheck (Ass expressionL expressionR) = do
    tcType <- statementExpressionCheck expressionL
    matchExpressionTypeMessage tcType expressionR
    ask

statementCheck (Incr expression) = statementExpressionCheck expression >>= matchType [TInt] >> ask

statementCheck (Decr expression) = statementExpressionCheck expression >>= matchType [TInt] >> ask

statementCheck (Ret expression) = matchReturnType =<< statementExpressionCheck expression

statementCheck (VRet) = matchReturnType TVoid

statementCheck (Cond expression statement) = conditionalCheck expression [statement]

statementCheck (CondElse expression statementTrue statementFalse) =
    conditionalCheck expression [statementTrue, statementFalse]

statementCheck (While expression statement) = conditionalCheck expression [statement]

statementCheck (For iteratorType (Ident identifier) expression statement) = do
    classTypeExistsCheck iteratorType
    (TArr arrayTCType) <- matchExpressionTypeMessage metaArray expression
    iteratorTCType     <- convertTypeToTCType iteratorType
    matchType [iteratorTCType] arrayTCType
    environment <- ask
    let newTypeMap = Map.insert identifier (arrayTCType, scope environment + 1) (typeMap environment)
    let statementBlockWrap = case statement of
            (BStmt _) -> statement
            _         -> BStmt (Block [statement])
    local (\env -> env { typeMap = newTypeMap }) (statementCheck statementBlockWrap)
    ask

statementCheck (SExp expression) = do
    _ <- statementExpressionCheck expression
    ask
