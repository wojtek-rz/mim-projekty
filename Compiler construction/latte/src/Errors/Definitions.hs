module Errors.Definitions where

import Bnfc.Abs
import Bnfc.Print (printTree)

data CodeFragment = StmtFragment Stmt | ExprFragment Expr | TopDefFragment TopDef | ArgFragment Arg | Position BNFC'Position | NoFragment

instance Show CodeFragment where
    show (StmtFragment stmt) = "In statement: " ++ printTree stmt ++ " " ++ printPos (hasPosition stmt)
    show (ExprFragment expr) = "In expression: " ++ printTree expr ++ " " ++ printPos (hasPosition expr)
    show (TopDefFragment topdef) = "In function:\n" ++ triquote (printTree topdef) ++ "\n" ++ printPos (hasPosition topdef)
    show (ArgFragment arg) = "In argument: " ++ quote (printTree arg) ++ " " ++ printPos (hasPosition arg)
    show (Position pos) = printPos pos
    show NoFragment = ""

class Error a where
    title :: a -> String
    description :: a -> String
    codeFragment :: a -> CodeFragment

printPos :: BNFC'Position -> String
printPos (Just (l, c)) = "[line " ++ show l ++ ", column " ++ show c ++ "]"
printPos Nothing = ""

quote :: String -> String
quote s = "\"" ++ s ++ "\""

triquote :: String -> String
triquote s = "\"\"\"\n" ++ s ++ "\"\"\""

brackets :: String -> String
brackets s = "[" ++ s ++ "]"

concatErr :: [String] -> String
concatErr = foldr (\x acc -> x ++ "\n" ++ acc) ""

showError :: Error a => a -> String
showError e = concatErr [
        brackets (title e),
        description e,
        show (codeFragment e)
    ]
