module TC_Returns (runReturnsCheck) where

import Latte.Abs

import Control.Monad.Reader
import Control.Monad.Trans.Except

import TC_Utils


---------------------
-- check functions --
---------------------
returnCheck :: Stmt -> TCMonad Bool
returnCheck (BStmt (Block statementList)) = foldM findReturn False statementList
    where
        findReturn accumulator statement = (||) accumulator <$> returnCheck statement

returnCheck (Ret _) = return True

returnCheck VRet = return True

returnCheck (Cond  ELitTrue statement) = returnCheck statement

returnCheck (CondElse ELitTrue  statementTrue _) = returnCheck statementTrue

returnCheck (CondElse ELitFalse _ statementFalse) = returnCheck statementFalse

returnCheck (CondElse _ statementTrue statementFalse) = do
    returnStatementTrue  <- returnCheck statementTrue
    returnStatementFalse <- returnCheck statementFalse
    return (returnStatementTrue && returnStatementFalse)

returnCheck (While ELitTrue statement) = returnCheck statement

returnCheck (SExp (EApp (Ident "error") _)) = return True

returnCheck _ = return False

classMemberReturnCheck :: ClsMem -> TCMonad Bool
classMemberReturnCheck (Attr _ (Ident _)) = return False

classMemberReturnCheck (Meth Void (Ident _) _ _) = return True

classMemberReturnCheck (Meth _ (Ident identifier) _ block) = do
    isValid <- returnCheck (BStmt block)
    if isValid
        then return False
        else throwTCMonad $ "No return value in method `" ++ identifier ++ "`\n"

topDefReturnCheck :: TopDef -> TCMonad ()
topDefReturnCheck (FnDef Void (Ident _         ) _ _    ) = return ()

topDefReturnCheck (FnDef _    (Ident identifier) _ block) = do
    isValid <- returnCheck (BStmt block)
    if isValid
        then return ()
        else throwTCMonad $ "No return value in function `" ++ identifier ++ "`\n"

topDefReturnCheck (ClsDef (Ident identifier) _ classMemberList) =
    mapM_ classMemberReturnCheck classMemberList `throwAdditionalMessage` errorMessage
    where
        errorMessage error =
            error ++
            "in the following class:\n" ++
            identifier ++
            "\n"

-------------------
-- run functions --
-------------------
runReturnsCheck :: [TopDef] -> TCMonad ()
runReturnsCheck = mapM_ topDefReturnCheck
