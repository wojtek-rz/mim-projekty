module CodeGenerator.DotGraph where

import CodeGenerator.IRCode
import qualified Data.Map as Map

class DotGraph a where
  toDot :: a -> String

instance DotGraph Function where
  toDot (Function name retType args blocks _ _) =
    let argNames = map varName args in
    "digraph " ++ name ++ " {\n"
      ++ "node [shape=plaintext];\n"
      ++ "args [label=\"" ++ show retType ++ " " ++ name ++ "(" ++ unwords argNames ++ ")\"];\n"
      ++ concatMap toDot (Map.elems blocks)
      ++ concatMap _genDotArrows (Map.elems blocks)
      ++ "}\n"

_genDotArrows :: BasicBlock -> String
_genDotArrows (BasicBlock label instrs _ _) =
  case reverse instrs of
    ((Branch _ label1 label2) : _) -> label ++ " -> " ++ label1 ++ " [label=\"true\"]; \n" ++ label ++ " -> " ++ label2 ++ " [label=\"false\"]; \n"
    ((GoTo label1) : _) -> label ++ " -> " ++ label1 ++ ";\n"
    _ -> ""

instance DotGraph BasicBlock where
  toDot (BasicBlock label instrs _ _) = label ++ " [\n" ++
    "label=<<TABLE BORDER=\"1\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n" ++
    "<TR><TD BGCOLOR=\"lightgray\" COLSPAN=\"2\">" ++ label ++ "</TD></TR>\n" ++
    concatMap (\instr -> "<TR><TD COLSPAN=\"2\">" ++ escapeDot (show instr) ++ "</TD></TR>\n") instrs
    ++ "</TABLE>>\n];\n"

escapeDot :: String -> String
escapeDot = concatMap replaceChar
  where
    replaceChar '&' = "&amp;"
    replaceChar '<' = "&lt;"
    replaceChar '>' = "&gt;"
    replaceChar '"' = "&quot;"
    replaceChar '\'' = "&apos;"
    replaceChar c   = [c]


generateDotFunction :: Program -> String -> String
generateDotFunction (Program functions _ _) functionName =
  case Map.lookup functionName functions of
    Just fun -> toDot fun
    Nothing -> error "No main function found"
