module Classes (saveAllClassesMembers, classDefinitionsCheck) where

import Data.List as List
import Data.Map as Map

import Control.Monad.Reader

import Latte.Abs

import Statements
import Utils

------------------------
-- exported functions --
------------------------
saveAllClassesMembers :: [TopDef] -> TCMonad TCEnvironment
saveAllClassesMembers topDefList = do
    environment <- ask
    foldM foldFunction environment topDefList
    where
        foldFunction :: TCEnvironment -> TopDef -> TCMonad TCEnvironment
        foldFunction env topDef = local (const env) $ saveClassMembers topDef

classDefinitionsCheck :: [TopDef] -> TCMonad TCEnvironment
classDefinitionsCheck topDefList = do
    environment <- ask
    foldM foldFunction environment topDefList
    where
        foldFunction :: TCEnvironment -> TopDef -> TCMonad TCEnvironment
        foldFunction env topDef = local (const env) $ classDefinitionCheck topDef

----------------------------------------------
-- save all class members support functions --
----------------------------------------------
extendsCycleCheck :: Variable -> [Variable] -> TCMonad TCEnvironment
extendsCycleCheck className encounteredClassList = do
    maybeClassParent <- getParentClass className
    case maybeClassParent of
        Nothing ->
            ask
        (Just classParent) ->
            if elem classParent encounteredClassList
                then
                    throwTCMonad $
                        "Detected class inheritence cycle for classes: " ++
                        intercalate " -> " encounteredClassList
                else extendsCycleCheck classParent (classParent : encounteredClassList)

classMemberExistsCheck :: Variable -> Map.Map Variable TCType -> TCMonad ()
classMemberExistsCheck memberName memberMap = do
    case Map.lookup memberName memberMap of
        Nothing -> return ()
        _       ->
            throwTCMonad $
                "Duplicate class member declaration: member `" ++
                memberName ++
                "` has already been declared"

saveClassMembers :: TopDef -> TCMonad TCEnvironment
saveClassMembers (FnDef _ _ _ _) = ask

saveClassMembers (ClsDef (Ident identifier) _ classMemberList) = do
    extendsCycleCheck identifier [identifier]
    environment <- ask
    environmentWithClassMembers <- foldM (foldFunction identifier) environment classMemberList
    return $ environment { classMap = classMap environmentWithClassMembers }
    where
        foldFunction :: Variable -> TCEnvironment -> ClsMem -> TCMonad TCEnvironment
        foldFunction ident env classMember = do
            local (const env) $ saveClassMember ident classMember `throwAdditionalMessage` errorMessage
            where
                errorMessage error = "inside class `" ++ ident ++ "`\n" ++ error

saveClassMember :: Variable -> ClsMem -> TCMonad TCEnvironment
saveClassMember className classMember = do
    case classMember of
        (Attr attributeType (Ident identifier)) ->
            saveClassMemberExecutor className identifier attributeType
        (Meth returnType    (Ident identifier) argumentList _) ->
            let methodType = Fun returnType (List.map (\(Arg argumentType _) -> argumentType) argumentList) in
                saveClassMemberExecutor className identifier methodType `throwAdditionalMessage` errorMessage identifier
                where
                    errorMessage ident error = "inside method `" ++ ident ++ "`\n" ++ error

saveClassMemberExecutor :: Variable -> Variable -> Type -> TCMonad TCEnvironment
saveClassMemberExecutor className memberName memberType = do
    environment     <- ask
    classDefinition <- getConfirmedClassDefinition className
    classMemberExistsCheck memberName (memberMap classDefinition)
    when (voidTypeCheck memberType) $
        throwTCMonad $
            "Cannot declare class member of type void"
    memberTCType <- convertTypeToTCType memberType
    let classDef = classDefinition { memberMap = Map.insert memberName memberTCType $ memberMap classDefinition }
    return $ environment { classMap = Map.insert className classDef (classMap environment) }

-----------------------------------------------
-- class definitions check support functions --
-----------------------------------------------
classDefinitionCheck :: TopDef -> TCMonad TCEnvironment
classDefinitionCheck (FnDef _ _ _ _) = ask

classDefinitionCheck (ClsDef (Ident identifier) _ classMemberList) = do
    environment <- ask
    environmentWithParentClassAttributes <- appendParentAttributes identifier
    environmentWithClassMembers <- foldM (appendAttribute identifier) environmentWithParentClassAttributes classMemberList
    foldM_ (classMethodCheck identifier) environmentWithClassMembers classMemberList
    ask

getClassAttributeList :: ClassDefinition -> TCMonad [(Variable, (TCType, Scope))]
getClassAttributeList classDefinition = do
    return $ List.map (\(identifier, tcType) -> (identifier, (tcType, 1))) (Map.toList $ memberMap classDefinition)

getAllAttributeList :: ClassDefinition -> TCMonad [(Variable, (TCType, Scope))]
getAllAttributeList classDefinition = do
    case extends classDefinition of
        Nothing -> do
            return []
        (Just parentClass) -> do
            parentClassDefinition    <- getConfirmedClassDefinition parentClass
            parentClassAttributeList <- getClassAttributeList parentClassDefinition
            otherClassAttributeList  <- getAllAttributeList parentClassDefinition
            return $ parentClassAttributeList ++ otherClassAttributeList

appendParentAttributes :: Variable -> TCMonad TCEnvironment
appendParentAttributes className = do
    classDefinition <- getConfirmedClassDefinition className
    attributeList   <- getAllAttributeList classDefinition
    environment     <- ask
    return environment { typeMap = Map.union (Map.fromList attributeList) $ typeMap environment }

appendAttribute :: Variable -> TCEnvironment -> ClsMem -> TCMonad TCEnvironment
appendAttribute identifier environment classMember = do
    local (const environment) $ appendAttributeExecutor identifier classMember `throwAdditionalMessage` errorMessage
    where
        errorMessage error = "inside class`" ++ identifier ++ "`\n" ++ error

appendAttributeExecutor :: Variable -> ClsMem -> TCMonad TCEnvironment
appendAttributeExecutor className (Attr attributeType (Ident identifier)) = do
    classDefinition <- getConfirmedClassDefinition className
    attributeTCType <- convertTypeToTCType attributeType
    environment     <- ask
    local (\env -> env { scope = 1 }) $ nameAlreadyInScopeCheck identifier
    return environment { typeMap = Map.insert identifier (attributeTCType, 1) $ typeMap environment }

appendAttributeExecutor className (Meth returnType (Ident identifier) argumentList _) = do
    classDefinition <- getConfirmedClassDefinition className
    methodTCType    <- convertTypeToTCType $ Fun returnType (List.map (\(Arg argumentType _) -> argumentType) argumentList)
    environment     <- ask
    return environment { typeMap = Map.insert identifier (methodTCType, 1) $ typeMap environment }


classMethodCheck :: Variable -> TCEnvironment -> ClsMem -> TCMonad TCEnvironment
classMethodCheck identifier environment classMember =
    local (const environment) $ performClassMethodCheck identifier classMember `throwAdditionalMessage` errorMessage
    where
        errorMessage error = "inside class `" ++ identifier ++ "`\n" ++ error

        performClassMethodCheck :: Variable -> ClsMem -> TCMonad TCEnvironment
        performClassMethodCheck className (Attr _ _) = ask

        performClassMethodCheck className (Meth returnType (Ident identifier) argumentList block) =
            classMethodCheckExecutor `throwAdditionalMessage` innerErrorMessage
            where
                innerErrorMessage error = "inside method `" ++ identifier ++ "`\n" ++ error

                classMethodCheckExecutor :: TCMonad TCEnvironment
                classMethodCheckExecutor = do
                    classDefinition <- getConfirmedClassDefinition className
                    case extends classDefinition of
                        Nothing -> do
                            return ()
                        (Just parentClass) -> do
                            parentClassDefinition <- getConfirmedClassDefinition parentClass
                            matchVirtualMethod classDefinition parentClassDefinition
                    argumentTypeList <- functionArgumentListCheck argumentList
                    let argTypeList = Map.insert "self" (TCls className, scope environment + 1) argumentTypeList
                    returnTCType <- convertTypeToTCType returnType
                    environment <- ask
                    let environmentWithMethodTypes = environment {
                        typeMap            = Map.union argTypeList (typeMap environment),
                        expectedReturnType = Just returnTCType
                    }
                    local (const environmentWithMethodTypes) $ statementCheck (BStmt block)
                    where
                        matchVirtualMethod :: ClassDefinition -> ClassDefinition -> TCMonad ()
                        matchVirtualMethod classDefinition parentClassDefinition = do
                            case Map.lookup identifier $ memberMap parentClassDefinition of
                                Nothing -> do
                                    return ()
                                (Just parentMemberType) -> do
                                    case Map.lookup identifier $ memberMap classDefinition of
                                        Nothing -> do
                                            throwTCMonad $
                                                "Impossible error: method `" ++
                                                identifier ++
                                                "` has already been checked for existence. It is currently being validated"
                                        (Just memberType) -> do
                                            matchType [parentMemberType] memberType
                                            return ()
