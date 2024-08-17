module Evaluator.Builtins where
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity
import Evaluator.LangData
import Builtins

implementation :: Builtins.Builtin -> LambdaFunc
implementation Builtins.PrintStr = do
    StringValue s <- getVarValue "s"
    liftIO $ putStrLn s
    return VoidValue

implementation Builtins.PrintInt = do
    IntValue i <- getVarValue "i"
    liftIO $ print i
    return VoidValue

implementation Builtins.PrintBool = do
    BoolValue b <- getVarValue "b"
    liftIO $ print b
    return VoidValue

implementation Builtins.IntToString = do
    IntValue i <- getVarValue "i"
    return $ StringValue $ show i

implementation Builtins.BoolToInt = do
    BoolValue b <- getVarValue "b"
    return $ IntValue $ if b then 1 else 0

implementation Builtins.Sqrt = do
    IntValue x <- getVarValue "x"
    return $ IntValue $ round (sqrt (fromIntegral x) :: Double)



getNameValue :: Builtins.Builtin -> (Name, Value)
getNameValue builtin = (Builtins.name builtin, LambdaValue (Builtins.args builtin) (Builtins.retType builtin) (implementation builtin) Map.empty)


emptyState :: EvalState
emptyState = EvalState False Map.empty Map.empty

-- Can't use addVar because of IO
entryState :: EvalState
entryState =
    runIdentity (execStateT (mapM_ (uncurry _addVar . getNameValue) Builtins.builtinsList ) emptyState)
    where
        _addVar varName val = do
            envValue <- gets env
            storeValue <- gets store
            let loc = Map.size storeValue
                envValue' = Map.insert varName loc envValue
                storeValue' = Map.insert loc val storeValue
                in modify (\s -> s{env = envValue', store = storeValue'}) :: StateT EvalState Identity ()