{-# LANGUAGE InstanceSigs #-}
module TypeChecker.LangTypes where

type Name = String

data LangType = IntType | BoolType | StringType | VoidType |
            FunType [LangArgType] LangType
            deriving (Eq)

data LangArgType = ValueArg LangType | RefArg LangType
    deriving (Eq)

instance Show LangArgType where
    show :: LangArgType -> String
    show (ValueArg value) = show value
    show (RefArg value) = "* " ++ show value

instance Show LangType where
    show :: LangType -> String
    show IntType = "Int"
    show BoolType = "Bool"
    show StringType = "String"
    show VoidType = "Void"
    show (FunType argTypes retType) =
        "(" ++ unwords (map show argTypes) ++ ") -> " ++ show retType
