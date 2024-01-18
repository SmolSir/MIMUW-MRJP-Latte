-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Latte.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Latte.Abs
import Latte.Lex

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'       { PT _ (TS _ 1)  }
  '!='      { PT _ (TS _ 2)  }
  '%'       { PT _ (TS _ 3)  }
  '&&'      { PT _ (TS _ 4)  }
  '('       { PT _ (TS _ 5)  }
  ')'       { PT _ (TS _ 6)  }
  ')null'   { PT _ (TS _ 7)  }
  '*'       { PT _ (TS _ 8)  }
  '+'       { PT _ (TS _ 9)  }
  '++'      { PT _ (TS _ 10) }
  ','       { PT _ (TS _ 11) }
  '-'       { PT _ (TS _ 12) }
  '--'      { PT _ (TS _ 13) }
  '.'       { PT _ (TS _ 14) }
  '/'       { PT _ (TS _ 15) }
  ':'       { PT _ (TS _ 16) }
  ';'       { PT _ (TS _ 17) }
  '<'       { PT _ (TS _ 18) }
  '<='      { PT _ (TS _ 19) }
  '='       { PT _ (TS _ 20) }
  '=='      { PT _ (TS _ 21) }
  '>'       { PT _ (TS _ 22) }
  '>='      { PT _ (TS _ 23) }
  '['       { PT _ (TS _ 24) }
  '[]'      { PT _ (TS _ 25) }
  ']'       { PT _ (TS _ 26) }
  'boolean' { PT _ (TS _ 27) }
  'class'   { PT _ (TS _ 28) }
  'else'    { PT _ (TS _ 29) }
  'extends' { PT _ (TS _ 30) }
  'false'   { PT _ (TS _ 31) }
  'for'     { PT _ (TS _ 32) }
  'if'      { PT _ (TS _ 33) }
  'int'     { PT _ (TS _ 34) }
  'new'     { PT _ (TS _ 35) }
  'return'  { PT _ (TS _ 36) }
  'string'  { PT _ (TS _ 37) }
  'true'    { PT _ (TS _ 38) }
  'void'    { PT _ (TS _ 39) }
  'while'   { PT _ (TS _ 40) }
  '{'       { PT _ (TS _ 41) }
  '||'      { PT _ (TS _ 42) }
  '}'       { PT _ (TS _ 43) }
  L_Ident   { PT _ (TV $$)   }
  L_integ   { PT _ (TI $$)   }
  L_quoted  { PT _ (TL $$)   }

%%

Ident :: { Latte.Abs.Ident }
Ident  : L_Ident { Latte.Abs.Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

String  :: { String }
String   : L_quoted { $1 }

Program :: { Latte.Abs.Program }
Program : ListTopDef { Latte.Abs.Program $1 }

TopDef :: { Latte.Abs.TopDef }
TopDef
  : Type Ident '(' ListArg ')' Block { Latte.Abs.FnDef $1 $2 $4 $6 }
  | 'class' Ident ClsExt '{' ListClsMem '}' { Latte.Abs.ClsDef $2 $3 $5 }

ListTopDef :: { [Latte.Abs.TopDef] }
ListTopDef : TopDef { (:[]) $1 } | TopDef ListTopDef { (:) $1 $2 }

ClsExt :: { Latte.Abs.ClsExt }
ClsExt
  : {- empty -} { Latte.Abs.NoExt }
  | 'extends' Ident { Latte.Abs.Ext $2 }

ClsMem :: { Latte.Abs.ClsMem }
ClsMem
  : Type Ident ';' { Latte.Abs.Attr $1 $2 }
  | Type Ident '(' ListArg ')' Block { Latte.Abs.Meth $1 $2 $4 $6 }

ListClsMem :: { [Latte.Abs.ClsMem] }
ListClsMem : {- empty -} { [] } | ClsMem ListClsMem { (:) $1 $2 }

Arg :: { Latte.Abs.Arg }
Arg : Type Ident { Latte.Abs.Arg $1 $2 }

ListArg :: { [Latte.Abs.Arg] }
ListArg
  : {- empty -} { [] }
  | Arg { (:[]) $1 }
  | Arg ',' ListArg { (:) $1 $3 }

Block :: { Latte.Abs.Block }
Block : '{' ListStmt '}' { Latte.Abs.Block $2 }

ListStmt :: { [Latte.Abs.Stmt] }
ListStmt : {- empty -} { [] } | Stmt ListStmt { (:) $1 $2 }

Stmt :: { Latte.Abs.Stmt }
Stmt
  : ';' { Latte.Abs.Empty }
  | Block { Latte.Abs.BStmt $1 }
  | Type ListItem ';' { Latte.Abs.Decl $1 $2 }
  | Expr8 '=' Expr ';' { Latte.Abs.Ass $1 $3 }
  | Expr8 '++' ';' { Latte.Abs.Incr $1 }
  | Expr8 '--' ';' { Latte.Abs.Decr $1 }
  | 'return' Expr ';' { Latte.Abs.Ret $2 }
  | 'return' ';' { Latte.Abs.VRet }
  | 'if' '(' Expr ')' Stmt { Latte.Abs.Cond $3 $5 }
  | 'if' '(' Expr ')' Stmt 'else' Stmt { Latte.Abs.CondElse $3 $5 $7 }
  | 'while' '(' Expr ')' Stmt { Latte.Abs.While $3 $5 }
  | 'for' '(' Type Ident ':' Expr ')' Stmt { Latte.Abs.For $3 $4 $6 $8 }
  | Expr ';' { Latte.Abs.SExp $1 }

Item :: { Latte.Abs.Item }
Item
  : Ident { Latte.Abs.NoInit $1 }
  | Ident '=' Expr { Latte.Abs.Init $1 $3 }

ListItem :: { [Latte.Abs.Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }

Type :: { Latte.Abs.Type }
Type
  : 'int' { Latte.Abs.Int }
  | 'string' { Latte.Abs.Str }
  | 'boolean' { Latte.Abs.Bool }
  | 'void' { Latte.Abs.Void }
  | Type '[]' { Latte.Abs.Arr $1 }
  | Ident { Latte.Abs.Cls $1 }

ListType :: { [Latte.Abs.Type] }
ListType
  : {- empty -} { [] }
  | Type { (:[]) $1 }
  | Type ',' ListType { (:) $1 $3 }

Expr8 :: { Latte.Abs.Expr }
Expr8
  : Ident { Latte.Abs.EVar $1 }
  | Expr7 '[' Expr ']' { Latte.Abs.EArr $1 $3 }
  | Expr7 '.' Ident { Latte.Abs.EAttr $1 $3 }
  | '(' Expr ')' { $2 }

Expr7 :: { Latte.Abs.Expr }
Expr7
  : Ident '(' ListExpr ')' { Latte.Abs.EApp $1 $3 }
  | Expr7 '.' Ident '(' ListExpr ')' { Latte.Abs.EMeth $1 $3 $5 }
  | 'new' Type EArrLen { Latte.Abs.ENew $2 $3 }
  | Expr8 { $1 }

EArrLen :: { Latte.Abs.EArrLen }
EArrLen
  : '[' Expr ']' { Latte.Abs.EArrLen $2 }
  | {- empty -} { Latte.Abs.EClsLen }

Expr6 :: { Latte.Abs.Expr }
Expr6
  : '(' Type ')null' { Latte.Abs.ENullCast $2 }
  | Integer { Latte.Abs.ELitInt $1 }
  | 'true' { Latte.Abs.ELitTrue }
  | 'false' { Latte.Abs.ELitFalse }
  | String { Latte.Abs.EString $1 }
  | Expr7 { $1 }

Expr5 :: { Latte.Abs.Expr }
Expr5
  : '-' Expr6 { Latte.Abs.Neg $2 }
  | '!' Expr6 { Latte.Abs.Not $2 }
  | Expr6 { $1 }

Expr4 :: { Latte.Abs.Expr }
Expr4
  : Expr4 MulOp Expr5 { Latte.Abs.EMul $1 $2 $3 } | Expr5 { $1 }

Expr3 :: { Latte.Abs.Expr }
Expr3
  : Expr3 AddOp Expr4 { Latte.Abs.EAdd $1 $2 $3 } | Expr4 { $1 }

Expr2 :: { Latte.Abs.Expr }
Expr2
  : Expr2 RelOp Expr3 { Latte.Abs.ERel $1 $2 $3 } | Expr3 { $1 }

Expr1 :: { Latte.Abs.Expr }
Expr1 : Expr2 '&&' Expr1 { Latte.Abs.EAnd $1 $3 } | Expr2 { $1 }

Expr :: { Latte.Abs.Expr }
Expr : Expr1 '||' Expr { Latte.Abs.EOr $1 $3 } | Expr1 { $1 }

ListExpr :: { [Latte.Abs.Expr] }
ListExpr
  : {- empty -} { [] }
  | Expr { (:[]) $1 }
  | Expr ',' ListExpr { (:) $1 $3 }

AddOp :: { Latte.Abs.AddOp }
AddOp : '+' { Latte.Abs.Plus } | '-' { Latte.Abs.Minus }

MulOp :: { Latte.Abs.MulOp }
MulOp
  : '*' { Latte.Abs.Times }
  | '/' { Latte.Abs.Div }
  | '%' { Latte.Abs.Mod }

RelOp :: { Latte.Abs.RelOp }
RelOp
  : '<' { Latte.Abs.LTH }
  | '<=' { Latte.Abs.LE }
  | '>' { Latte.Abs.GTH }
  | '>=' { Latte.Abs.GE }
  | '==' { Latte.Abs.EQU }
  | '!=' { Latte.Abs.NE }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

