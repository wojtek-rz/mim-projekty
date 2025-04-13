module TypeCheck.Definitions where

import TypeCheck.Errors
import TypeCheck.LangTypes
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except

type VarEnv = M.Map String LangType

type FuncEnv = M.Map String FunctionType

type Namespace = S.Set String

data TypeCheckEnv = TypeState 
    {
        currentFunctionType :: FunctionType, 
        variables :: VarEnv,
        functions :: FuncEnv,
        classes :: M.Map String ClassTypeDef,
        localNamespace :: Namespace
    }


type TypeCheckMonad result = ExceptT TypeCheckErrors (StateT TypeCheckEnv IO) result