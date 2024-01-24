module Backend (runCompiler) where

import Data.List as List
import Data.Map as Map
import Data.Bits as Bits

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

import Latte.Abs

import C_Classes
import C_Statements
import C_Utils


----------------------
-- helper functions --
----------------------
translateTopDef :: TopDef -> CMonad InstructionPrepend
translateTopDef (FnDef returnType (Ident identifier) argumentList block) = do
    modify (\state -> state {
        localsCount = 0,
        returnLabel = "ret_" ++ identifier
    })
    let argList = Map.fromList $
            zipWith
                (\(Arg argumentType (Ident ident)) index -> (ident, (Parameter index, argumentType)))
                argumentList
                [1 .. ]
    (_, code) <- local
        (\environment -> environment { variableMap = argList })
        (translateStatement (BStmt block))
    state <- get
    return $
        instructionListMerge [
            LABEL $ FunctionLabel identifier,
            HEADER,
            STACK_ALLOCATION $ localsCount state
            ] .
        code .
        instructionListMerge [
            LABEL $ FunctionLabel $ returnLabel state,
            FOOTER,
            ZERO_INSTRUCTION RET
            ]

translateTopDef (ClsDef (Ident identifier) _ classMemberList) = do
    code <- mapM (translateMethods identifier) classMemberList
    return $ List.foldr (.) id code

translateProgram :: InstructionPrepend -> [TopDef] -> CMonad InstructionPrepend
translateProgram vmTablesCode topDefList = do
    topDefsCode <- foldM foldFunction id topDefList
    stringMap   <- gets stringMap
    let stringLabelList = List.map LABEL $ Map.elems stringMap
    return $
        instructionListMerge [
            ENTRY,
            DATA
        ] .
        instructionListMerge (stringLabelList) .
        vmTablesCode .
        instructionListAdd (TEXT) .
        topDefsCode
    where
        foldFunction :: InstructionPrepend -> TopDef -> CMonad InstructionPrepend
        foldFunction accumulatedCode topDef = do
            generatedCode <- translateTopDef topDef
            return (accumulatedCode . generatedCode)

-------------------
-- run functions --
-------------------
runCompiler (Program prog) =
    evalStateT
        (runReaderT (go prog) (CEnvironment 0 Map.empty Nothing))
        (CState 0 0 "" Map.empty Map.empty Map.empty predefinedFunctionMap)
    where
        go prog = do
            saveClassListMembers prog
            virtualMethodTablesCode <- translateAllVirtualMethodTables
            flip ($) [] <$> translateProgram virtualMethodTablesCode prog >>= instructionListUnpack

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
