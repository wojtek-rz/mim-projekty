{-# LANGUAGE InstanceSigs #-}
module TypeChecker.Exceptions (module TypeChecker.Exceptions) where
import TypeChecker.LangTypes (LangType)
import Bnfc.Abs
import Exceptions

type VarName = String
data TypeCheckException =
    VariableNotDeclaredException VarName BNFC'Position |
    NotAFucntionException LangType VarName Expr |
    TypeMismatchException LangType LangType Expr |
    WrongNumberOfArgumentsException Int Int Expr |
    FunctionWrongReturnTypeException LangType LangType Expr |
    FunctionNotVoidReturnTypeException LangType LangType BNFC'Position |
    RefArgumentIsNotVariableException Expr |
    VoidTypeDeclarationException VarName Expr |
    NoReturn LangType BNFC'Position |
    InternalError String BNFC'Position


expectedActualType :: LangType -> LangType -> String
expectedActualType expected actual = 
    concat [
        "Expected type: " ++ quote (show expected),
        "\n",
        "Actual type: " ++ quote (show actual)]

instance EmiliaException TypeCheckException where
    title :: TypeCheckException -> String
    title (VariableNotDeclaredException {}) = "Variable not declared"
    title (NotAFucntionException {}) = "Variable is not a function"
    title (TypeMismatchException {}) = "Type mismatch"
    title (WrongNumberOfArgumentsException {}) = "Wrong number of arguments in function call"
    title (FunctionWrongReturnTypeException {}) = "Function wrong return type"
    title (FunctionNotVoidReturnTypeException {}) = "Function not void return type"
    title (RefArgumentIsNotVariableException {}) = "Reference argument is not a variable"
    title (VoidTypeDeclarationException {}) = "Void type declaration"
    title (NoReturn {}) = "Missing return in lambda function"
    title (InternalError {}) = "Internal error"

    description :: TypeCheckException -> String
    description (VariableNotDeclaredException varName _) =
        "Variable name: " ++ quote varName ++ " is not declared."
    description (NotAFucntionException varType varName _) =
        "Variable name: " ++ quote varName ++ " is not a function.\nVariable type: " ++ quote (show varType)
    description (TypeMismatchException expectedType actualType _) =
        expectedActualType expectedType actualType
    description (WrongNumberOfArgumentsException expected actual _) =
        "Expected number of arguments: " ++ show expected ++ "\nActual number of arguments: " ++ show actual
    description (FunctionWrongReturnTypeException expectedType actualType _) =
        expectedActualType expectedType actualType
    description (FunctionNotVoidReturnTypeException expectedType actualType _) =
        expectedActualType expectedType actualType
    description (RefArgumentIsNotVariableException _) = "Reference argument is not a variable."
    description (VoidTypeDeclarationException varName _) =
        "Variable name: " ++ quote varName ++ " is declared with Void type."
    description (NoReturn shouldReturn _) =
        "Lambda function should return " ++ quote (show shouldReturn) ++ " but didn't."
    description (InternalError msg _) = "Message: " ++ quote msg
    
    codeFragment :: TypeCheckException -> CodeFragment
    codeFragment (VariableNotDeclaredException _ pos) = Position pos
    codeFragment (NotAFucntionException _ _ expr) = ExprFragment expr
    codeFragment (TypeMismatchException _ _ expr) = ExprFragment expr
    codeFragment (WrongNumberOfArgumentsException _ _ expr) = ExprFragment expr
    codeFragment (FunctionWrongReturnTypeException _ _ expr) = ExprFragment expr
    codeFragment (FunctionNotVoidReturnTypeException _ _ pos) = Position pos
    codeFragment (RefArgumentIsNotVariableException expr) = ExprFragment expr
    codeFragment (VoidTypeDeclarationException _ expr) = ExprFragment expr
    codeFragment (NoReturn _ pos) = Position pos
    codeFragment (InternalError _ pos) = Position pos

instance Show TypeCheckException where
    show = showEmiliaException