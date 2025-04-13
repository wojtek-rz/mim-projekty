module TypeCheck.Builtins where

import TypeCheck.LangTypes

data Builtins = PrintInt | PrintString | ThrowError | ReadInt | ReadString

ident :: Builtins -> String
ident PrintInt = "printInt"
ident PrintString = "printString"
ident ThrowError = "error"
ident ReadInt = "readInt"
ident ReadString = "readString"

builtinType :: Builtins -> FunctionType
builtinType PrintInt = FunctionType VoidType [IntType]
builtinType PrintString = FunctionType VoidType [StringType]
builtinType ThrowError = FunctionType VoidType []
builtinType ReadInt = FunctionType IntType []
builtinType ReadString = FunctionType StringType []

toDeclList :: [(String, FunctionType)]
toDeclList = map (\b -> (ident b, builtinType b)) [PrintInt, PrintString, ThrowError, ReadInt, ReadString]
