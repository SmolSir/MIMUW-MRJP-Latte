module Classes (saveAllClassesMembers, classDefinitionsCheck) where

import Data.Map as Map

import Control.Monad.Reader

import Latte.Abs

import Utils

------------------------
-- exported functions --
------------------------
saveAllClassesMembers :: [TopDef] -> TCMonad TCEnvironment
saveAllClassesMembers topDefList = undefined

classDefinitionsCheck :: [TopDef] -> TCMonad TCEnvironment
classDefinitionsCheck topDefList = undefined
