module Frontend (runStaticAnalysis) where

import Data.List as List
import Data.Map as Map

import Control.Monad.Reader

import Latte.Abs

import Utils
import Statements
import Returns


----------------------
-- helper functions --
----------------------
saveTopDefTypes :: TCEnvironment -> TopDef -> TCMonad TCEnvironment
saveTopDefTypes environment topDef = local (const environment) $ saveTopDef topDef
    where
        saveTopDef :: TopDef -> TCMonad TCEnvironment
        saveTopDef (FnDef returnType (Ident identifier) argumentList _) = do
            nameAlreadyInScopeCheck identifier
            tcType      <- convertTypeToTCType $ Fun returnType (List.map (\(Arg argumentType _) -> argumentType) argumentList)
            environment <- ask
            return $ environment { typeMap = Map.insert identifier (tcType, 0) (typeMap environment) }



---------------------
-- check functions --
---------------------
mainPresenceCheck :: TCMonad ()
mainPresenceCheck = do
    typeMap <- asks typeMap
    case Map.lookup "main" typeMap of
        Nothing                -> throwTCMonad $ "`main` function must be declared"
        Just (TFun [] TInt, _) -> return ()
        Just (functionType, _) ->
            throwTCMonad $ "`main` function must be of type `int`, but was declared `" ++ show functionType ++ "`"

functionDefinitionsCheck :: [TopDef] -> TCMonad TCEnvironment
functionDefinitionsCheck  functionDefinitionList = do
    environment <- ask
    foldM runFunctionDefinitionsCheck environment functionDefinitionList
    where
        runFunctionDefinitionsCheck :: TCEnvironment -> TopDef -> TCMonad TCEnvironment
        runFunctionDefinitionsCheck env functionDefinition =
            local (const env) $ functionDefinitionCheck functionDefinition

        functionDefinitionCheck :: TopDef -> TCMonad TCEnvironment
        functionDefinitionCheck (FnDef returnType (Ident identifier) argumentList block) = do
            tcReturnType    <- convertTypeToTCType returnType
            environment     <- ask
            argumentTypeMap <- functionArgumentListCheck argumentList
            let env = environment {
                typeMap            = Map.union argumentTypeMap (typeMap environment),
                expectedReturnType = Just tcReturnType
            }
            local (const env) $ statementCheck (BStmt block) `throwAdditionalMessage` errorMessage
            ask
            where
                errorMessage error = "inside function `" ++ identifier ++ "`\n" ++ error

functionArgumentListCheck :: [Arg] -> TCMonad TypeMap
functionArgumentListCheck argumentList = do
    scope    <- asks scope
    typeList <- mapM (
        \(Arg argumentType (Ident identifier)) -> do
            tcType <- convertTypeToTCType argumentType
            if tcType == TVoid
                then
                    throwTCMonad $
                    "Invalid function parameter of type `void`: `" ++
                    identifier ++
                    "`"
                else
                    return (identifier, (tcType, scope + 1))
        )
        argumentList
    let typeMap = fromList typeList
    if length typeMap == length typeList
        then
            return typeMap
        else
            throwTCMonad "Duplicate function's argument names"

-------------------
-- run functions --
-------------------
runStaticAnalysis (Program prog) =
    runReaderT (parseTopDefList prog) $ TCEnvironment predefinedFunctionList 0 Nothing
    where
        predefinedFunctionList :: Map.Map Variable (TCType, Scope)
        predefinedFunctionList = Map.fromList [
            ("error"      , (TFun []        TVoid,   0)),
            ("readInt"    , (TFun []        TInt,    0)),
            ("readString" , (TFun []        TString, 0)),
            ("printInt"   , (TFun [TInt]    TVoid,   0)),
            ("printString", (TFun [TString] TVoid,   0))]

        parseTopDefList :: [TopDef] -> TCMonad TCEnvironment
        parseTopDefList topDefList = do
            environment            <- ask
            environmentWithTopDefs <- foldM saveTopDefTypes environment prog
            local (const environmentWithTopDefs) mainPresenceCheck
            local (const environmentWithTopDefs) (functionDefinitionsCheck prog)
            runReturnsCheck prog
            ask
