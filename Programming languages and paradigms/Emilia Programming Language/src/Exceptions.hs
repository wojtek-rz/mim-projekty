module Exceptions where
import Bnfc.Abs
import Bnfc.Print (printTree)

data CodeFragment = StmtFragment Stmt | ExprFragment Expr | Position BNFC'Position | NoFragment

printPos :: BNFC'Position -> String
printPos (Just (l, c)) = "[line " ++ show l ++ ", column " ++ show c ++ "]"
printPos Nothing = ""

instance Show CodeFragment where
    show (StmtFragment stmt) = "In statement: " ++ quote (printTree stmt) ++ " " ++ printPos (hasPosition stmt)
    show (ExprFragment expr) = "In expression: " ++ quote (printTree expr) ++ " " ++ printPos (hasPosition expr)
    show (Position pos) = printPos pos
    show NoFragment = ""

class EmiliaException a where
    title :: a -> String
    description :: a -> String
    codeFragment :: a -> CodeFragment

quote :: String -> String
quote s = "\"" ++ s ++ "\""

brackets :: String -> String
brackets s = "[" ++ s ++ "]"

concatErr :: [String] -> String
concatErr = foldr (\x acc -> x ++ "\n" ++ acc) ""

showEmiliaException :: EmiliaException a => a -> String
showEmiliaException e = concatErr [
        brackets (title e),
        description e,
        show (codeFragment e)
    ]