module Utils where

import Data.List as List
import Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except

import Latte.Abs
import Latte.Print


------------------
-- types & data --
------------------
type Variable = String
type Scope = Integer
type TypeMap = Map.Map Variable (TCType, Scope)
type TCMonad a = ReaderT TCEnvironment (ExceptT String IO) a

data ClassDefinition = ClassDefinition {
    extends :: Maybe Variable,
    memberMap :: Map.Map Variable TCType
} deriving (Show)

data TCEnvironment = TCEnvironment {
    typeMap :: TypeMap,
    classMap :: Map.Map Variable ClassDefinition,
    scope :: Scope,
    expectedReturnType :: Maybe TCType
} deriving (Show)

data TCType
    = TInt
    | TString
    | TBool
    | TVoid
    | TArr TCType
    | TCls Variable
    | TFun [TCType] TCType
    deriving (Eq)


--------------------
-- meta variables --
--------------------
metaArray :: TCType
metaArray = TArr TVoid

metaClass :: TCType
metaClass = TCls ""

----------------------
-- helper functions --
----------------------
showTypeList :: Show a => [a] -> String
showTypeList typeList = intercalate ", " (List.map show typeList)

instance Show TCType where
    show TInt    = "int"
    show TString = "string"
    show TBool   = "bool"
    show TVoid   = "void"
    show (TArr arrayType) = show arrayType ++ "[]"
    show (TCls className) = "class `" ++ className
    show (TFun argumentTypeList returnType) =
        "(" ++ showTypeList argumentTypeList ++ ") -> " ++ show returnType

convertTypeToTCType :: Type -> TCMonad TCType
convertTypeToTCType Int  = return TInt

convertTypeToTCType Str  = return TString

convertTypeToTCType Bool = return TBool

convertTypeToTCType Void = return TVoid

convertTypeToTCType (Arr arrayType) = TArr <$> convertTypeToTCType arrayType

convertTypeToTCType cls@(Cls (Ident identifier)) = do
    classTypeExistsCheck cls
    return $ TCls identifier

convertTypeToTCType (Fun returnType argumentTypeList) =
    TFun <$> mapM convertTypeToTCType argumentTypeList <*> convertTypeToTCType returnType

throwTCMonad :: String -> TCMonad a
throwTCMonad = lift . throwE

throwAdditionalMessage :: TCMonad a -> (String -> String) -> TCMonad a
throwAdditionalMessage actual message = catchError actual (throwTCMonad . message)

matchType :: [TCType] -> TCType -> TCMonad ()
matchType [TCls parentCls] (TCls cls) = do
    compatibleClassList <- getCompatibleClassList cls
    when (notElem parentCls compatibleClassList) $ throwTCMonad errorMessage
    where
        errorMessage =
            "Incompatible class types. Expected type: " ++
            show parentCls ++
            " but the actual type was: " ++
            show cls

matchType [expectedType] actualType =
    when (expectedType /= actualType) $ throwTCMonad $ errorMessage
    where
        errorMessage =
            "Expected type: " ++
            show expectedType ++
            " but the actual type was: " ++
            show actualType

matchType expectedTypeList cls@(TCls _) =
    when (notElem metaClass expectedTypeList) $ throwTCMonad $ errorMessage
    where
        errorMessage =
            "Expected type to be one of: " ++
            showTypeList expectedTypeList ++
            " but the actual type was: " ++
            show cls

matchType expectedTypeList actualType =
    when (notElem actualType expectedTypeList) $ throwTCMonad $ errorMessage
    where
        errorMessage =
            "Expected type to be one of: " ++
            showTypeList expectedTypeList ++
            " but the actual type was: " ++
            show actualType

matchReturnType :: TCType -> TCMonad TCEnvironment
matchReturnType actualType = do
    expectedType <- asks expectedReturnType
    case expectedType of
        Nothing       -> throwTCMonad "Usage of return outside of a function"
        Just expected -> matchType [expected] actualType `throwAdditionalMessage` errorMessage
    ask
    where
        errorMessage error = "Invalid function return type: " ++ error

--------------------------
-- type check functions --
--------------------------
voidTypeCheck :: Type -> Bool
voidTypeCheck Void = True

voidTypeCheck (Arr arrayType) = voidTypeCheck arrayType

voidTypeCheck _ = False

------------------------------
-- variable check functions --
------------------------------
variableTypeScope :: Variable -> TCMonad (Maybe (TCType, Scope))
variableTypeScope variable = do
    environment <- asks typeMap
    return $ Map.lookup variable environment

variableType :: Variable -> TCMonad TCType
variableType variable = do
    typeScope <- variableTypeScope variable
    case typeScope of
        Nothing          -> throwTCMonad $ "Usage of undeclared variable `" ++ variable ++ "`\n"
        Just (tcType, _) -> return tcType

--------------------------
-- name check functions --
--------------------------
nameAlreadyInScopeCheck :: Variable -> TCMonad ()
nameAlreadyInScopeCheck variable = do
    currentScope <- asks scope
    typeScope    <- variableTypeScope variable
    case typeScope of
        Nothing -> return ()
        Just (_, variableScope) ->
            when (currentScope == variableScope) $ throwTCMonad $ "Already declared variable `" ++ variable ++ "`\n"

---------------------------
-- class check functions --
---------------------------
classExistsCheck :: Variable -> TCMonad ()
classExistsCheck cls = do
    classDefinition <- getMaybeClassDefinition cls
    case classDefinition of
        Nothing -> throwTCMonad $ "Usage of undeclared class `" ++ cls ++ "`\n"
        _       -> return ()

classTypeExistsCheck :: Type -> TCMonad ()
classTypeExistsCheck (Arr arrayType) = classTypeExistsCheck arrayType

classTypeExistsCheck (Cls (Ident identifier)) = classExistsCheck identifier

classTypeExistsCheck _ = return ()

getMaybeClassDefinition :: Variable -> TCMonad (Maybe ClassDefinition)
getMaybeClassDefinition cls = do
    classMap <- asks classMap
    return $ Map.lookup cls classMap

getConfirmedClassDefinition :: Variable -> TCMonad ClassDefinition
getConfirmedClassDefinition cls = do
    classMap <- asks classMap
    case Map.lookup cls classMap of
        Nothing                -> throwTCMonad $ errorMessage
        (Just classDefinition) -> return classDefinition
        where
            errorMessage =
                "Impossible error: class `" ++
                cls ++
                "` has already been checked for existence. It is currently having its members checked"

getParentClass :: Variable -> TCMonad (Maybe Variable)
getParentClass cls = do
    classExistsCheck cls
    classMap <- asks classMap
    case Map.lookup cls classMap of
        Nothing                -> throwTCMonad $ errorMessage
        (Just classDefinition) -> return $ extends classDefinition
        where
            errorMessage =
                "Impossible error: class `" ++
                cls ++
                "` has already been checked for existence"

getCompatibleClassList :: Variable -> TCMonad [Variable]
getCompatibleClassList cls =
    getCompatibleList [cls] cls
    where
        getCompatibleList :: [Variable] -> Variable -> TCMonad [Variable]
        getCompatibleList compatibleList cls = do
            parentCls <- getParentClass cls
            case parentCls of
                Nothing       -> return compatibleList
                (Just parent) -> getCompatibleList (parent : compatibleList) parent
