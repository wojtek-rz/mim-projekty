{-# LANGUAGE InstanceSigs #-}
module Evaluator.Exceptions 
    (module Evaluator.Exceptions,
    module Exceptions) where
import Bnfc.Abs
import Exceptions

data EvalException = 
    DivisionByZeroException Expr |
    InternalError String CodeFragment

instance EmiliaException EvalException where
    title :: EvalException -> String
    title (DivisionByZeroException {}) = "Division by zero"
    title (InternalError {}) = "Internal error"

    description :: EvalException -> String
    description (DivisionByZeroException _) = "Expression has value 0."
    description (InternalError msg _) = "Message: " ++ msg

    codeFragment :: EvalException -> CodeFragment
    codeFragment (DivisionByZeroException expr) = ExprFragment expr 
    codeFragment (InternalError _ fragment) =  fragment
    

instance Show EvalException where
    show = showEmiliaException
