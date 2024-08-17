module Evaluator.LangData where

import qualified Data.Map as Map
import Bnfc.Abs
import Control.Monad.State
import Control.Monad.Except
import Evaluator.Exceptions
import Bnfc.Print (printTree)

type Loc = Int
type Name = String

type Env = Map.Map Name Loc
type Store = Map.Map Loc Value

-- LambdaFunc is a monadic function that returns what the function should return
type LambdaFunc = EvalM Value

data EvalState = EvalState{ignoreStmts :: Bool,
                           env :: Env, store :: Store}
type EvalM result = ExceptT EvalException (StateT EvalState IO) result

data Value =
    IntValue Integer |
    BoolValue Bool |
    StringValue String |
    LambdaValue [Arg] Type LambdaFunc Env | 
    VoidValue

data ArgValue = 
    Value Value |
    Ref Loc

instance Show Value where
    show (IntValue i) = show i
    show (BoolValue b) = show b
    show (StringValue s) = show s
    show (LambdaValue args retType _ _) =
        concat [
            "(",
            printTree args,
            ") -> ",
            printTree retType
        ]
    show VoidValue = ""

instance Eq Value where
    (IntValue i1) == (IntValue i2) = i1 == i2
    (BoolValue b1) == (BoolValue b2) = b1 == b2
    (StringValue s1) == (StringValue s2) = s1 == s2
    VoidValue == VoidValue = True
    _ == _ = False

showArg :: Arg -> String
showArg = printTree

returnLoc :: Loc
returnLoc = -1

setReturnValue :: Value -> EvalM ()
setReturnValue = setLocValue returnLoc

getReturnValue :: EvalM Value
getReturnValue = getLocValue returnLoc

setIgnoreStmts :: Bool -> EvalM ()
setIgnoreStmts val = modify (\s -> s{ignoreStmts = val})


---------------------- UTILITIES FUNCTIONS ---------------------- 

addArg :: Arg -> ArgValue -> EvalM ()
addArg (FunArg _ (Ident name) _) value = do
    case value of
        Value val -> addVar name val
        Ref loc -> do
            addVarLoc name loc


getNewLoc :: EvalM Loc
getNewLoc = do
    storeValue <- gets store
    return $ Map.size storeValue

addVar :: Name -> Value -> EvalM ()
addVar name val = do
    loc <- getNewLoc
    envValue <- gets env
    storeValue <- gets store
    let envValue' = Map.insert name loc envValue
        storeValue' = Map.insert loc val storeValue
        in modify (\s -> s{env = envValue', store = storeValue'})


setVarValue :: Name -> Value -> EvalM ()
setVarValue name val = do
    envValue <- gets env
    case Map.lookup name envValue of
        Just loc -> do
            setLocValue loc val
        Nothing -> throwError $ InternalError ("Variable " ++ name ++ " not found in environment") NoFragment

setLocValue :: Loc -> Value -> EvalM ()
setLocValue loc val = do
    storeValue <- gets store
    let storeValue' = Map.insert loc val storeValue
        in modify (\s -> s{store = storeValue'})

addVarLoc :: Name -> Loc -> EvalM ()
addVarLoc name loc = do
    envValue <- gets env
    let envValue' = Map.insert name loc envValue
        in modify (\s -> s{env = envValue'})

getVarValue :: Name -> EvalM Value
getVarValue name = do
    loc <- getVarLoc name
    storeValue <- gets store
    case Map.lookup loc storeValue of
        Just val -> return val
        Nothing -> throwError $ InternalError ("Variable " ++ name ++ " not found in store") NoFragment

getVarLoc :: Name -> EvalM Loc
getVarLoc name = do
    envValue <- gets env
    case Map.lookup name envValue of
        Just loc -> return loc
        Nothing -> throwError $ InternalError ("Location of " ++ name ++ " not found in environment") NoFragment

getLocValue :: Loc -> EvalM Value
getLocValue loc = do
    storeValue <- gets store
    case Map.lookup loc storeValue of
        Just val -> return val
        Nothing -> throwError $ InternalError ("Variable of loc " ++ show loc ++ " not found in store") NoFragment


withLocalEnv :: Env -> EvalM a -> EvalM a
withLocalEnv newEnv action = do
    envSave <- gets env
    modify (\s -> s{env = newEnv})
    result <- action
    modify (\s -> s{env = envSave})
    return result

withRestoredEnv :: EvalM a -> EvalM a
withRestoredEnv action = do
    oldEnv <- gets env
    result <- action
    modify (\s -> s{env = oldEnv})
    return result

setEnv :: Env -> EvalM ()
setEnv newEnv = modify (\s -> s{env = newEnv})

setEnvWithName :: Env -> Name -> EvalM ()
setEnvWithName newEnv name = do
    loc <- getVarLoc name
    let newEnv' = Map.insert name loc newEnv
        in modify (\s -> s{env = newEnv'})