-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Bnfc.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Bnfc.Abs
import Bnfc.Lex

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!='     { PT _ (TS _ 1)  }
  '%'      { PT _ (TS _ 2)  }
  '&&'     { PT _ (TS _ 3)  }
  '('      { PT _ (TS _ 4)  }
  ')'      { PT _ (TS _ 5)  }
  '*'      { PT _ (TS _ 6)  }
  '+'      { PT _ (TS _ 7)  }
  '++'     { PT _ (TS _ 8)  }
  ','      { PT _ (TS _ 9)  }
  '-'      { PT _ (TS _ 10) }
  '->'     { PT _ (TS _ 11) }
  '/'      { PT _ (TS _ 12) }
  ':'      { PT _ (TS _ 13) }
  ':='     { PT _ (TS _ 14) }
  ';'      { PT _ (TS _ 15) }
  '<'      { PT _ (TS _ 16) }
  '<-'     { PT _ (TS _ 17) }
  '<='     { PT _ (TS _ 18) }
  '='      { PT _ (TS _ 19) }
  '>'      { PT _ (TS _ 20) }
  '>='     { PT _ (TS _ 21) }
  'Bool'   { PT _ (TS _ 22) }
  'False'  { PT _ (TS _ 23) }
  'Int'    { PT _ (TS _ 24) }
  'Str'    { PT _ (TS _ 25) }
  'True'   { PT _ (TS _ 26) }
  'Void'   { PT _ (TS _ 27) }
  'else'   { PT _ (TS _ 28) }
  'if'     { PT _ (TS _ 29) }
  'not'    { PT _ (TS _ 30) }
  'pass'   { PT _ (TS _ 31) }
  'return' { PT _ (TS _ 32) }
  'while'  { PT _ (TS _ 33) }
  '{'      { PT _ (TS _ 34) }
  '|'      { PT _ (TS _ 35) }
  '||'     { PT _ (TS _ 36) }
  '}'      { PT _ (TS _ 37) }
  L_Ident  { PT _ (TV _)    }
  L_integ  { PT _ (TI _)    }
  L_quoted { PT _ (TL _)    }

%%

Ident :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Ident) }
Ident  : L_Ident { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Ident (tokenText $1)) }

Integer :: { (Bnfc.Abs.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (Bnfc.Abs.BNFC'Position, String) }
String   : L_quoted { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Program) }
Program
  : ListStmt { (fst $1, Bnfc.Abs.ProgramMain (fst $1) (snd $1)) }

Block :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Block) }
Block
  : '{' ListStmt '}' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.StmtBlock (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListStmt :: { (Bnfc.Abs.BNFC'Position, [Bnfc.Abs.Stmt]) }
ListStmt
  : {- empty -} { (Bnfc.Abs.BNFC'NoPosition, []) }
  | Stmt { (fst $1, (:[]) (snd $1)) }
  | Stmt ';' ListStmt { (fst $1, (:) (snd $1) (snd $3)) }

Stmt :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Stmt) }
Stmt
  : Ident ':=' Expr { (fst $1, Bnfc.Abs.Decl (fst $1) (snd $1) (snd $3)) }
  | 'pass' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Pass (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | Ident '<-' Expr { (fst $1, Bnfc.Abs.Assign (fst $1) (snd $1) (snd $3)) }
  | 'if' Expr ':' Block { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Cond (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | 'if' Expr ':' Block 'else' ':' Block { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.CondElse (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $7)) }
  | 'while' Expr ':' Block { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.While (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | Expr { (fst $1, Bnfc.Abs.ExprStmt (fst $1) (snd $1)) }
  | 'return' Expr { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Ret (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.VRet (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }

Expr6 :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Expr) }
Expr6
  : Ident { (fst $1, Bnfc.Abs.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, Bnfc.Abs.ELitInt (fst $1) (snd $1)) }
  | 'True' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.ELitTrue (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'False' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.ELitFalse (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | '(' ListArg ')' '->' Type '(' Expr ')' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.ELambdaShort (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $5) (snd $7)) }
  | '(' ListArg ')' '->' Type ':' Block { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.ELambda (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $5) (snd $7)) }
  | Ident '(' ListExpr ')' { (fst $1, Bnfc.Abs.EApp (fst $1) (snd $1) (snd $3)) }
  | String { (fst $1, Bnfc.Abs.EString (fst $1) (snd $1)) }
  | '(' Expr ')' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), (snd $2)) }

Arg :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Arg) }
Arg
  : Ident '|' ArgType { (fst $1, Bnfc.Abs.FunArg (fst $1) (snd $1) (snd $3)) }

ListArg :: { (Bnfc.Abs.BNFC'Position, [Bnfc.Abs.Arg]) }
ListArg
  : {- empty -} { (Bnfc.Abs.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

Expr5 :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Expr) }
Expr5
  : Expr5 '++' Expr6 { (fst $1, Bnfc.Abs.Concat (fst $1) (snd $1) (snd $3)) }
  | '-' Expr6 { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Neg (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'not' Expr6 { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Not (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, Bnfc.Abs.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, Bnfc.Abs.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, Bnfc.Abs.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr2 '=' Expr3 { (fst $1, Bnfc.Abs.EEquals (fst $1) (snd $1) (snd $3)) }
  | Expr2 '!=' Expr3 { (fst $1, Bnfc.Abs.ENequals (fst $1) (snd $1) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Expr) }
Expr1
  : Expr2 '&&' Expr1 { (fst $1, Bnfc.Abs.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Expr) }
Expr
  : Expr1 '||' Expr { (fst $1, Bnfc.Abs.EOr (fst $1) (snd $1) (snd $3)) }
  | Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (Bnfc.Abs.BNFC'Position, [Bnfc.Abs.Expr]) }
ListExpr
  : {- empty -} { (Bnfc.Abs.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

Type :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.Type) }
Type
  : 'Int' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Int (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'Str' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Str (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'Bool' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Bool (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'Void' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Void (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | '(' ListArgType ')' '->' Type { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Fun (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $5)) }

ListArgType :: { (Bnfc.Abs.BNFC'Position, [Bnfc.Abs.ArgType]) }
ListArgType
  : {- empty -} { (Bnfc.Abs.BNFC'NoPosition, []) }
  | ArgType { (fst $1, (:[]) (snd $1)) }
  | ArgType ',' ListArgType { (fst $1, (:) (snd $1) (snd $3)) }

ArgType :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.ArgType) }
ArgType
  : Type { (fst $1, Bnfc.Abs.ValueArgType (fst $1) (snd $1)) }
  | '*' Type { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.RefArgType (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

AddOp :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.AddOp) }
AddOp
  : '+' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Plus (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Minus (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.MulOp) }
MulOp
  : '*' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Times (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Div (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.Mod (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (Bnfc.Abs.BNFC'Position, Bnfc.Abs.RelOp) }
RelOp
  : '<' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.LTH (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.LE (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.GTH (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1), Bnfc.Abs.GE (uncurry Bnfc.Abs.BNFC'Position (tokenLineCol $1))) }

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

-- Entrypoints

pProgram :: [Token] -> Err Bnfc.Abs.Program
pProgram = fmap snd . pProgram_internal
}

