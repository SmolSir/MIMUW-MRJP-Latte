module Frontend (runStaticAnalysis) where

import Data.List as List
import Data.Map as Map

import Control.Monad.Reader

import Latte.Abs

import TC_Classes
import TC_Returns
import TC_Statements
import TC_Utils


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

        saveTopDef (ClsDef (Ident identifier) classExtends _) = do
            classDefinition <- getMaybeClassDefinition identifier
            case classDefinition of
                Nothing -> do
                    let extends = extendsUnwrap classExtends
                    let classDefinition = ClassDefinition { extends = extends, memberMap = Map.empty }
                    environment <- ask
                    return $ environment { classMap = Map.insert identifier classDefinition (classMap environment) }
                    where
                        extendsUnwrap :: ClsExt -> Maybe Variable
                        extendsUnwrap extends = case extends of
                            NoExt -> Nothing
                            Ext (Ident parentIdentifier) -> (Just parentIdentifier)
                _ -> do
                    throwTCMonad $
                        "Duplicate class declaration: class `" ++
                        identifier ++
                        "` has already been declared"

---------------------
-- check functions --
---------------------
mainPresenceCheck :: TCMonad ()
mainPresenceCheck = do
    typeMap <- asks typeMap
    case Map.lookup "main" typeMap of
        Nothing                -> throwTCMonad $ "`main` function must be declared \n"
        Just (TFun [] TInt, _) -> return ()
        Just (functionType, _) ->
            throwTCMonad $ "`main` function must be of type int, but was declared " ++ show functionType ++ "\n"

functionDefinitionsCheck :: [TopDef] -> TCMonad TCEnvironment
functionDefinitionsCheck functionDefinitionList = do
    environment <- ask
    foldM runFunctionDefinitionsCheck environment functionDefinitionList
    where
        runFunctionDefinitionsCheck :: TCEnvironment -> TopDef -> TCMonad TCEnvironment
        runFunctionDefinitionsCheck env functionDefinition =
            local (const env) $ functionDefinitionCheck functionDefinition

        functionDefinitionCheck :: TopDef -> TCMonad TCEnvironment
        functionDefinitionCheck (ClsDef _ _ _) = ask
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

-------------------
-- run functions --
-------------------
runStaticAnalysis (Program prog) =
    runReaderT (parseTopDefList prog) $ TCEnvironment predefinedFunctionList Map.empty 0 Nothing
    where
        predefinedFunctionList :: Map.Map Variable (TCType, Scope)
        predefinedFunctionList = Map.fromList [
            ("error"        , (TFun []                 TVoid  , 0)),
            ("readInt"      , (TFun []                 TInt   , 0)),
            ("readString"   , (TFun []                 TString, 0)),
            ("printInt"     , (TFun [TInt]             TVoid  , 0)),
            ("printString"  , (TFun [TString]          TVoid  , 0)),
            ("concatString" , (TFun [TString, TString] TString, 0)),
            ("compareString", (TFun [TString, TString] TInt   , 0))]

        parseTopDefList :: [TopDef] -> TCMonad TCEnvironment
        parseTopDefList topDefList = do
            environment            <- ask
            environmentWithTopDefs <- foldM saveTopDefTypes environment prog
            local (const environmentWithTopDefs) mainPresenceCheck
            environmentWithClassMembers <- local (const environmentWithTopDefs) (saveAllClassesMembers prog)
            environmentWithClassDefinitions <- local (const environmentWithClassMembers) (classDefinitionsCheck prog)
            local (const environmentWithClassDefinitions) (functionDefinitionsCheck prog)
            runReturnsCheck prog
            ask
