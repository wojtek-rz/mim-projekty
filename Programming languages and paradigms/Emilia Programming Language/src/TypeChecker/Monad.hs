module TypeChecker.Monad where
import TypeChecker.Exceptions
import TypeChecker.LangTypes
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

type ReturnType = LangType
type VarTypes = Map.Map Name LangType

data TypeState = TypeState {returnOccurred :: Bool, returnType :: ReturnType, variables :: VarTypes}
type TypeCheckM result = ExceptT TypeCheckException (StateT TypeState IO) result
