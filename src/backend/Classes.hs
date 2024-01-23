module Classes where

import Data.Map as Map

import Control.Monad.Reader
import Control.Monad.State

import Latte.Abs

import Statements
import Utils


----------------------
-- helper functions --
----------------------
getParentClassMemberList :: Maybe Variable -> CMonad [([(Variable, Type)], [(Variable, (Type, Variable))])]
getParentClassMemberList Nothing = return [([], [])]

getParentClassMemberList (Just className) = do
    classDefinitionMap <- gets classMap
    case Map.lookup className classDefinitionMap of
        Nothing -> do
            throwCMonad $
                "Impossible error: every class has its parent class"
        (Just classDefinition) -> do
            accumulatorList <- getParentClassMemberList $ extends classDefinition
            return $ (
                Map.toList $ attributeMap classDefinition,
                map
                    (\(variable, returnType) -> (variable, (returnType, className)))
                    (Map.toList $ methodMap classDefinition)
            ) : accumulatorList

foldClassMember :: ([(Variable, Type)], [(Variable, Type)]) -> ClsMem -> ([(Variable, Type)], [(Variable, Type)])
foldClassMember (attributeList, methodList) member = do
    case member of
        Attr attributeType (Ident identifier) -> do
            return ((identifier, returnType) : attributeList, methodList)
        Meth returnType (Ident identifier) argumentList _ -> do
            let methodType = Fun returnType (map (\(Arg argumentType _) -> argumentType) argumentList)
            return (attributeList, (identifier, methodType) : methodList)

saveClassMembers :: TopDef -> CMonad ()
saveClassMembers (FnDef returnType (Ident identifier) _ _) = do
    modify (\state -> state { functionReturnTypeMap = Map.insert identifier returnType (functionReturnTypeMap state) })
    return ()

saveClassMembers (ClsDef (Ident identifier) classExtends classMemberList) = do
    classDefinitionMap <- gets classMap
    (attributeList, methodList) <- foldM foldClassMember ([], []) classMemberList
    let classDefinition = ClassDefinitionData {
        extends      = extendsUnwrap classExtends,
        attributeMap = Map.fromList attributeList,
        methodMap    = Map.fromList methodList
    }
    modify (\state -> state { classMap = Map.insert identifier classDefinition classDefinitionMap })
    where
        extendsUnwrap :: ClsExt -> Maybe Variable
        extendsUnwrap clsExtends = case clsExtends of
            NoExt -> Nothing
            Ext (Ident parentClassName) -> (Just parentClassName)


translateVirtualMethodTable :: (Variable, ClassDefinitionData) -> CMonad ()
translateVirtualMethodTable (className, _) = do
    parentMemberList <- getParentClassMemberList (Just className)
    let (parentAttributeList, parentMethodList) = unzip parentMemberList
    let virtualAttrList =
        zipWith
            (\(attributeName, attributeType) index -> (attributeName, (Attribute index, attributeType)))
            (concat $ reverse parentAttributeList)
            [1 .. ]
    let parentMethList =
        concat $ reverse parentMethodList
    let orderedParentMethList =
        map fst $ nubBy (\(methodL, _) (methodR, _) -> methodL == methodR) parentMethList
    let parentMethMap =
        Map.fromList parentMethList
    let virtualMethList =
        zipWith
            (\(methodName, (methodType, clsName)) index -> (methodName, (methodType, index, clsName)))
            (map (`findMethodTypeAndClass` parentMethMap) orderedParentMethList)
            [0 .. ]
    let virtualMethodTable = VMTable {
        virtualAttributeMap = Map.fromList virtualAttrList,
        virtualMethodList   = virtualMethList
    }
    modify (\state -> state { vmtableMap = Map.insert className virtualMethodTable (vmtableMap state) })
    where
        findMethodTypeAndClass :: Variable -> Map.Map Variable (Type, Variable) -> (Variable, (Type, Variable))
        findMethodTypeAndClass methodName typeClassMap = do
            case Map.lookup methodName typeClassMap of
                Nothing ->
                    error  $
                        "Impossible error: existence of method `" ++
                        methodName ++
                        "` inside its class has already been confirmed by the typechecker"
                (Just (methodType, clsName)) ->
                    (methodName, (methodType, className))

------------------------
-- exported functions --
------------------------
saveClassListMembers :: [TopDef] -> CMonad ()
saveClassListMembers = mapM_ saveClassMembers

translateMethods :: Variable -> ClsMem -> CMonad InstructionPrepend
translateMethods _ (Attr _ _) = return id

translateMethods className (Meth returnType (Ident identifier) argumentList block) = do
    modify (\state -> state {
        localsCount           = 0,
        returnLabel           = "ret_" ++ getMethodLabel className identifier,
        functionReturnTypeMap = Map.insert identifier returnType (functionReturnTypeMap state)
    })
    virtualMethodTableMap <- gets vmtableMap
    case Map.lookup className virtualMethodTableMap of
        Nothing ->
            throwCMonad $
                "Impossible error: existence of VM table in class `" ++
                className ++
                "` has already been confirmed by the typechecker"
        (Just virtualMethodTable) -> do
            (_, generatedCode) <- do
                let argMap = Map.fromList $
                    zipWith
                        (\(Arg argumentType (Ident ident)) index -> (ident, (Parameter index, argumentType)))
                        argumentList
                        [2 .. ]
                let argMapIncludeSelf = Map.insert "self" (Parameter 1, Cls (Ident className)) argMap
                local
                    (\environment -> environment {
                        variableMap = Map.union (virtualAttributeMap virtualMethodTable) argMapIncludeSelf,
                        insideClass = (Just className)
                    })
                    (translateStatement (BStmt block))
            state <- get
            return $
                instructionListMerge [
                    LABEL $ FunctionLabel $ getMethodLabel className identifier,
                    HEADER,
                    STACK_ALLOCATION $ localsCount state
                ] .
                generatedCode .
                instructionListMerge [
                    LABEL $ FunctionLabel $ returnLabel state,
                    FOOTER,
                    ZERO_INSTRUCTION RET
                ]

translateAllVirtualMethodTables :: CMonad InstructionPrepend
translateAllVirtualMethodTables = do
    classDefinitionMap <- gets classMap
    mapM_ translateVirtualMethodTable $ Map.toList classDefinitionMap
    virtualMethodTableMap <- gets vmtableMap
    let virtualMethodLabelList =
        map
            (\(className, vmTable) -> (className, map labelVirtualMethod (virtualMethodList vmTable)))
            (Map.toList virtualMethodTableMap)
    let virtualMethLabelList =
        filter (\(_, vmList) -> vmList /= []) virtualMethodLabelList
    let instructionList =
        map (uncurry translateVirtualMethodLabel) virtualMethLabelList
    return $ foldr (.) id instructionList
    where
        translateVirtualMethodLabel :: Variable -> [String] -> InstructionPrepend
        translateVirtualMethodLabel clsName methodList = instructionListMerge [
            LABEL $ FunctionLabel (getVirtualMethodTableLabel clsName),
            VM_TABLE methodList
        ]

        labelVirtualMethod :: (Variable, (Type, Integer, Variable)) -> String
        labelVirtualMethod (methodName, (_, _, clsName)) = getMethodLabel clsName methodName
