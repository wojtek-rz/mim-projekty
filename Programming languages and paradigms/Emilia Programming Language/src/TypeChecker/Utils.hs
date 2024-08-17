
module TypeChecker.Utils where
import TypeChecker.Exceptions
import TypeChecker.LangTypes
import TypeChecker.Monad
import Bnfc.Abs
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Builtins

-- Translation from BNFC Arg to LangArgType
-- Ours: LangType, LangArgType
-- BNFC: Type, ArgType, Arg
getAnnotationType :: Type -> LangType
getAnnotationType (Int _) = IntType

getAnnotationType (Bool _) = BoolType

getAnnotationType (Str _) = StringType

getAnnotationType (Void _) = VoidType

getAnnotationType (Fun _ args retType) = do
    let argTypes = map toLangArgType args
    let retType' = getAnnotationType retType
        in FunType argTypes retType'

getArgLangArgType :: Arg -> LangArgType
getArgLangArgType (FunArg _ _ t) = toLangArgType t

getArgLangType :: Arg -> LangType
getArgLangType (FunArg _ _ t) = _toType (toLangArgType t)

getArgName :: Arg -> Name
getArgName (FunArg _ (Ident name) _) = name

toLangArgType :: ArgType -> LangArgType
toLangArgType (ValueArgType _ t) = ValueArg (getAnnotationType t)
toLangArgType (RefArgType _ t) = RefArg (getAnnotationType t)

_toType :: LangArgType -> LangType
_toType (ValueArg t) = t
_toType (RefArg t) = t

emptyTypeState :: TypeState
emptyTypeState = TypeState False VoidType Map.empty

entryTypeState :: TypeState
entryTypeState =
    let insertBuiltin builtin = Map.insert (Builtins.name builtin) (getLambdaType (Builtins.args builtin) (Builtins.retType builtin))
        vars = foldr insertBuiltin Map.empty Builtins.builtinsList
    in TypeState False VoidType vars

setReturnValue :: LangType -> TypeCheckM ()
setReturnValue t = modify (\s -> s {returnType = t})

getVarType :: Name -> TypeCheckException -> TypeCheckM LangType
getVarType name e = do
    env <- gets variables
    case Map.lookup name env of
        Just t -> return t
        Nothing -> throwError e

addVarType :: Name -> LangType -> TypeCheckM ()
addVarType name t = do
    env <- gets variables
    let env' = Map.insert name t env
    modify (\s -> s {variables = env'})


checkLangTypesEqual :: LangType -> LangType -> Expr -> TypeCheckM ()
checkLangTypesEqual expected actual expr =
    if expected == actual then return ()
    else throwError $ TypeMismatchException expected actual expr

checkReturnOccurred :: BNFC'Position -> TypeCheckM ()
checkReturnOccurred pos = do
    retType <- gets returnType
    occurred <- gets returnOccurred
    if (retType == VoidType) || occurred then return ()
    else throwError $ NoReturn retType pos

setReturnOccurred :: TypeCheckM ()
setReturnOccurred = do
    modify (\s -> s{returnOccurred = True})


withRestoredState :: TypeCheckM a -> TypeCheckM a
withRestoredState action = do
    oldState <- get
    result <- action
    put oldState
    return result

addArgsToState :: [Arg] -> TypeCheckM ()
addArgsToState args =
    let argNames = map getArgName args
        argTypes = map getArgLangType args
    in mapM_ (uncurry addVarType) (zip argNames argTypes)


getLambdaType :: [Arg] -> Type -> LangType
getLambdaType args retType = FunType (map getArgLangArgType args) (getAnnotationType retType)


runWithLocalState :: TypeState -> TypeCheckM a -> TypeCheckM a
runWithLocalState newState action = do
    oldState <- get
    put newState
    result <- action
    put oldState
    return result
