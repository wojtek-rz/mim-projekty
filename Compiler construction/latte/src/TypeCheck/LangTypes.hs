{-# LANGUAGE InstanceSigs #-}
module TypeCheck.LangTypes where
import Bnfc.Abs
import Debug.Trace (trace)
import BnfcUtils (getIdent)

type ArrayType = (LangType, Integer)
type ClassTypeDef = [(String, LangType)]
type ClassTypeDecl = String

-- SizedString and Pointer used only in the compiler, not in typecheck
data LangType = IntType | BoolType | StringType | VoidType | ArrayType LangType | ClassType ClassTypeDecl | SizedString Integer | Pointer LangType |
            FunType LangType [LangType] 
            deriving (Eq, Ord)

instance Show LangType where
    show :: LangType -> String
    show IntType = "Int"
    show BoolType = "Bool"
    show StringType = "String"
    show VoidType = "Void"
    show (FunType retType argTypes) = 
        "FunType " ++ show argTypes ++ " -> " ++ show retType
    show (ArrayType t) = show t ++ "[]"
    show (ClassType classType) = "Class " ++ classType
    show (SizedString n) = "char[" ++ show n ++ "]"
    show (Pointer t) = show t ++ "*"

data FunctionType = FunctionType {
    returns::LangType,
    args::[LangType]
}
    deriving (Eq)

instance Show FunctionType where
    show :: FunctionType -> String
    show (FunctionType retType argTypes) = 
        "FunctionType " ++ show argTypes ++ " -> " ++ show retType


fromAstType :: Type -> LangType
fromAstType (Int _) = IntType
fromAstType (Str _) = StringType
fromAstType (Bool _) = BoolType
fromAstType (Void _) = VoidType
fromAstType (Fun _ retType argTypes) =
    FunType (fromAstType retType) (map fromAstType argTypes)
fromAstType (Arr _ t) = ArrayType (fromAstType t)
fromAstType (Cls _ ident) = ClassType (getIdent ident)

fromAstArg :: Arg -> LangType
fromAstArg (Arg _ t _) = fromAstType t

functionFromAst :: TopDef -> FunctionType
functionFromAst (FnDef _ retType _ args _) = 
    FunctionType (fromAstType retType) (map fromAstArg args)

classFromAst :: TopDef -> ClassTypeDef
classFromAst (ClsDef _ _ items) = map fromAstItem items
    where
        fromAstItem :: ClsDefItem -> (String, LangType)
        fromAstItem (AttrDef _ t (Ident name)) = (name, fromAstType t)

getClassType :: [(String, LangType)] -> String -> Maybe LangType
getClassType [] name = Nothing
getClassType ((n, t):xs) name = if n == name then Just t else getClassType xs name 

getClassAttr :: [(String, LangType)] -> String -> Maybe (LangType, Integer)
getClassAttr classAttrs name = go classAttrs name 0
  where
    go [] _ _ = Nothing
    go ((n, t):xs) name index
      | n == name = Just (t, index)
      | otherwise = go xs name (index + 1)