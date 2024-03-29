-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Latte.

module Latte.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = Program [TopDef]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TopDef
    = FnDef Type Ident [Arg] Block | ClsDef Ident ClsExt [ClsMem]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ClsExt = NoExt | Ext Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ClsMem = Attr Type Ident | Meth Type Ident [Arg] Block
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = Arg Type Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Block = Block [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass Expr Expr
    | Incr Expr
    | Decr Expr
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | For Type Ident Expr Stmt
    | SExp Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type
    = Int | Str | Bool | Void | Arr Type | Cls Ident | Fun Type [Type]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = EVar Ident
    | EArr Expr Expr
    | EAttr Expr Ident
    | EApp Ident [Expr]
    | EMeth Expr Ident [Expr]
    | ENew Type EArrLen
    | ENullCast Type
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data EArrLen = EArrLen Expr | EClsLen
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AddOp = Plus | Minus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MulOp = Times | Div | Mod
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

