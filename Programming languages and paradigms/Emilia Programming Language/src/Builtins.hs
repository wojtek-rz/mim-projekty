module Builtins where
import Bnfc.Abs

data Builtin = PrintStr | PrintInt | PrintBool | IntToString | BoolToInt | Sqrt 
    deriving (Eq, Show, Enum, Bounded)

builtinsList :: [Builtin]
builtinsList = [minBound..maxBound]

name :: Builtin -> String
name PrintStr = "printStr"
name PrintInt = "printInt"
name PrintBool = "printBool"
name IntToString = "intToStr"
name BoolToInt = "boolToInt"
name Sqrt = "sqrt"

args :: Builtin -> [Arg]
args PrintStr = [FunArg Nothing (Ident "s") (ValueArgType Nothing (Str Nothing))] 
args PrintInt = [FunArg Nothing (Ident "i") (ValueArgType Nothing (Int Nothing))] 
args PrintBool = [FunArg Nothing (Ident "b") (ValueArgType Nothing (Bool Nothing))]
args IntToString = [FunArg Nothing (Ident "i") (ValueArgType Nothing (Int Nothing))] 
args BoolToInt = [FunArg Nothing (Ident "b") (ValueArgType Nothing (Bool Nothing))]
args Sqrt = [FunArg Nothing (Ident "x") (ValueArgType Nothing (Int Nothing))]

retType :: Builtin -> Type
retType PrintStr = Void Nothing
retType PrintInt = Void Nothing
retType PrintBool = Void Nothing
retType IntToString = Str Nothing
retType BoolToInt = Int Nothing
retType Sqrt = Int Nothing
