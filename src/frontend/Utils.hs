module Utils where

import Data.List as List
import Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except

import Latte.Abs

------------------
-- types & data --
------------------
type Variable = String
type Scope = Integer
type TypeMap = Map.Map Variable (TCType, Scope)
type TCMonad a = ReaderT TCEnvironment (ExceptT String IO) a

data TCEnvironment = TCEnvironment {
    typeMap :: TypeMap,
    scope :: Scope,
    expectedReturnType :: Maybe TCType
} deriving (Show)

data TCType = TInt | TString | TBool | TVoid | TFun [TCType] TCType deriving (Eq)

----------------------
-- helper functions --
----------------------
showTypeList :: Show a => [a] -> String
showTypeList = intercalate ", " . map show

instance Show TCType where
    show TInt    = "int"
    show TString = "string"
    show TBool   = "bool"
    show TVoid   = "void"
    show (TFun argumentTypeList returnType) =
        "(" ++ showTypeList argumentTypeList ++ ") -> " ++ show returnType

convertTypeToTCType :: Type -> TCMonad TCType
convertTypeToTCType Int    = return TInt
convertTypeToTCType String = return TString
convertTypeToTCType Bool   = return TBool
convertTypeToTCType Void   = return TVoid
convertTypeToTCType (Fun returnType argumentTypeList) =
    TFun <$> mapM convertTypeToTCType argumentTypeList <*> convertTypeToTCType returnType

matchType :: [TCType] -> TCType -> TCMonad ()
matchType [expectedType] actualType =
    when (expectedType /= actualType) $ throwTCMonad $ errorMessage
    where
        errorMessage =
            "Expected type: " ++
            show expectedType ++
            " but the actual type was: " ++
            show actualType
matchType expectedTypeList actualType =
    when (notElem actualType expectedTypeList) $ throwTCMonad $ errorMessage
    where
        errorMessage =
            "Expected type to be one of: " ++
            showTypeList expectedTypeList ++
            " but the actual type was: " ++
            show actualType

matchReturnType :: TCType -> TCMonad ()
matchReturnType actualType = do
    expectedType <- asks expectedReturnType
    case expectedType of
        Nothing       -> throwTCMonad "Usage of return outside of a function"
        Just expected -> matchType [expected] actualType `throwAdditionalMessage` errorMessage
    ask
    where
        errorMessage error = "Invalid function return type: " ++ error

throwTCMonad :: String -> TCMonad a
throwTCMonad = lift . throwE

throwAdditionalMessage :: TCMonad a -> (String -> String) -> TCMonad a
throwAdditionalMessage actual message = catchError actual (throwTCMonad . message)

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
        Nothing        -> throwTCMonad $ "Usage of undeclared variable `" ++ variable ++ "`"
        Just (type, _) -> return type

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
            when (currentScope == variableScope) $ throwTCMonad $ "Already declared variable `" ++ variable ++ "`"
