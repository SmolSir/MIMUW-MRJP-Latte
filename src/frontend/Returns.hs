module Returns (runReturnsCheck) where

import Latte.Abs

import Utils

import Control.Monad.Reader
import Control.Monad.Trans.Except


runReturnsCheck :: [TopDef] -> TCMonad ()
runReturnsCheck = mapM_ topDefReturnCheck
    where
        returnCheck :: Stmt -> TCMonad Bool
        returnCheck (Ret _) = return True
        returnCheck VRet    = return True
        returnCheck (CondElse ELitTrue  statementTrue _)              = returnCheck statementTrue
        returnCheck (CondElse ELitFalse _             statementFalse) = returnCheck statementFalse
        returnCheck (CondElse _         statementTrue statementFalse) = do
            returnStatementTrue  <- returnCheck statementTrue
            returnStatementFalse <- returnCheck statementFalse
            return (returnStatementTrue && returnStatementFalse)
        returnCheck (Cond  ELitTrue statement) = returnCheck statement
        returnCheck (While ELitTrue statement) = returnCheck statement
        returnCheck (BStmt (Block statementList)) = foldM findReturn False statementList
            where
                findReturn accumulator statement = (||) accumulator <$> returnCheck statement
        returnCheck _ = return False

        topDefReturnCheck :: TopDef -> TCMonad ()
        topDefReturnCheck (FnDef Void (Ident _         ) _ _    ) = return ()
        topDefReturnCheck (FnDef _    (Ident identifier) _ block) = do
            isValid <- returnCheck (BStmt block)
            if isValid
                then return ()
                else throwTCMonad $ "No return value in function `" ++ identifier ++ "`\n"
