{-# LANGUAGE InstanceSigs #-}
module TypeCheck.Errors where
import Errors.Definitions

import Bnfc.Abs

type Identificator = String
type ActualTypeString = String
type ExceptedTypeString = String

type ExpectedNumber = Int
type ActualNumber = Int

data TypeCheckErrors =
    UnexpectedExpressionType Expr ActualTypeString ExceptedTypeString |
    UnexpectedVariableType Stmt Identificator ActualTypeString ExceptedTypeString |
    WrongTypeForOperator Expr String ActualTypeString ExceptedTypeString |
    WrongTypesForOperator Expr String ActualTypeString ExceptedTypeString |
    VariableNotFound BNFC'Position Identificator |
    FunctionNotFound Expr Identificator |
    WrongReturnType Stmt ActualTypeString ExceptedTypeString |
    InternalError BNFC'Position String |
    WrongNumberOfArguments Expr Identificator ActualNumber ExpectedNumber |
    FunctionAlreadyDefined TopDef Identificator |
    ParameterAlreadyDefined Arg Identificator |
    IllegalType BNFC'Position ActualTypeString | 
    VariableAlreadyDefined Stmt Identificator |
    ConditionalDeclaration Stmt |
    MainNotPresent |
    MainWrongType ActualTypeString ExceptedTypeString |
    IntegerOverflow Expr |
    AttributeNotFound BNFC'Position Identificator Identificator |
    ClassAlreadyDefined TopDef Identificator |
    ClassNotFound BNFC'Position Identificator


instance Error TypeCheckErrors where
    title :: TypeCheckErrors -> String
    title (UnexpectedExpressionType {}) = "Unexpected expression type"
    title (UnexpectedVariableType {}) = "Unexpected variable type"
    title (WrongTypeForOperator {}) = "Wrong type for operator"
    title (WrongTypesForOperator {}) = "Wrong types for operator"
    title (VariableNotFound _ _) = "Variable not found"
    title (FunctionNotFound _ _) = "Function not found"
    title (WrongReturnType {}) = "Wrong return type"
    title (InternalError _ _) = "Internal error"
    title (WrongNumberOfArguments {}) = "Wrong number of arguments"
    title (FunctionAlreadyDefined _ _) = "Function already defined"
    title (ParameterAlreadyDefined _ _) = "Parameter already defined"
    title (IllegalType _ _) = "Illegal type"
    title (VariableAlreadyDefined _ _) = "Variable already defined"
    title (ConditionalDeclaration _) = "Conditional declaration"
    title MainNotPresent = "Main function not present"
    title (MainWrongType _ _) = "Main function has wrong type"
    title (IntegerOverflow _) = "Integer overflow"
    title (AttributeNotFound _ _ _) = "Attribute not found"
    title (ClassAlreadyDefined _ _) = "Class already defined"
    title (ClassNotFound _ _) = "Class not found"
    

    description :: TypeCheckErrors -> String
    description (UnexpectedExpressionType _ actualType expectedType) =
        "Unexpected type of expression. Expected " ++ quote expectedType ++ ", but got " ++ quote actualType
    description (UnexpectedVariableType _ ident actualType expectedType) =
        "Unexpected type of variable " ++ quote ident ++ ".  Expected " ++ quote expectedType ++ ", but got " ++ quote actualType
    description (WrongTypeForOperator _ operator actualType expectedType) =
        "Wrong type for operator " ++ quote operator ++ ". Expected " ++ quote expectedType ++ ", but got " ++ quote actualType
    description (WrongTypesForOperator _ operator actualType expectedType) =
        "Wrong types for operator " ++ quote operator ++ ". Expected " ++ quote expectedType ++ ", but got " ++ quote actualType
    description (VariableNotFound _ ident) =
        "Variable " ++ quote ident ++ " not found"
    description (FunctionNotFound _ ident) =
        "Function " ++ quote ident ++ " not found"
    description (WrongReturnType _ actualType expectedType) =
        "Wrong return type. Expected " ++ quote expectedType ++ ", but got " ++ quote actualType
    description (InternalError _ message) =
        "Internal error: " ++ message
    description (WrongNumberOfArguments _ ident actual expected) =
        "Wrong number of arguments for function " ++ quote ident ++ ". Expected " ++ show expected ++ ", but got " ++ show actual
    description (FunctionAlreadyDefined _ ident) =
        "Function " ++ quote ident ++ " already defined"
    description (ParameterAlreadyDefined _ ident) =
        "Parameter " ++ quote ident ++ " already defined"
    description (IllegalType _ actualType) =
        "Illegal type: " ++ quote actualType
    description (VariableAlreadyDefined _ ident) =
        "Variable " ++ quote ident ++ " already defined"
    description (ConditionalDeclaration _) =
        "Conditional declaration"
    description MainNotPresent =
        "Main function not present"
    description (MainWrongType actualType expectedType) =
        "Main function has wrong type. Expected " ++ quote expectedType ++ ", but got " ++ quote actualType
    description (IntegerOverflow expr) =
        "Integer overflow in expression " ++ quote (show expr)
    description (AttributeNotFound _ cls attr) =
        "Attribute " ++ quote attr ++ " not found in class " ++ quote cls
    description (ClassAlreadyDefined _ ident) =
        "Class " ++ quote ident ++ " already defined"
    description (ClassNotFound _ ident) =
        "Class " ++ quote ident ++ " not found"

    codeFragment :: TypeCheckErrors -> CodeFragment
    codeFragment (UnexpectedExpressionType expr _ _) = ExprFragment expr
    codeFragment (UnexpectedVariableType stmt _ _ _) = StmtFragment stmt
    codeFragment (WrongTypeForOperator expr _ _ _) = ExprFragment expr
    codeFragment (WrongTypesForOperator expr _ _ _) = ExprFragment expr
    codeFragment (VariableNotFound pos _) = Position pos
    codeFragment (FunctionNotFound expr _) = ExprFragment expr
    codeFragment (WrongReturnType stmt _ _) = StmtFragment stmt
    codeFragment (InternalError pos _) = Position pos
    codeFragment (WrongNumberOfArguments expr _ _ _) = ExprFragment expr
    codeFragment (FunctionAlreadyDefined topdef _) = TopDefFragment topdef
    codeFragment (ParameterAlreadyDefined arg _) = ArgFragment arg
    codeFragment (IllegalType pos _) = Position pos
    codeFragment (VariableAlreadyDefined stmt _) = StmtFragment stmt
    codeFragment (ConditionalDeclaration stmt) = StmtFragment stmt
    codeFragment MainNotPresent = NoFragment
    codeFragment (MainWrongType _ _) = NoFragment
    codeFragment (IntegerOverflow expr) = ExprFragment expr
    codeFragment (AttributeNotFound pos _ _) = Position pos
    codeFragment (ClassAlreadyDefined topdef _) = TopDefFragment topdef
    codeFragment (ClassNotFound pos _) = Position pos
