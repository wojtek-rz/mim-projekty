{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use when" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeCheck.TypeChecker where

import Bnfc.Abs
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import TypeCheck.Builtins
import TypeCheck.Definitions
import TypeCheck.LangTypes
import BnfcUtils (getIdent)
import TypeCheck.Errors
import Errors.Definitions
import Control.Monad(when)
import Bnfc.Print (printTree)

emptyState :: TypeCheckEnv
emptyState =
  TypeState
    { currentFunctionType = FunctionType VoidType [],
      variables = M.empty,
      functions = M.empty,
      classes = M.empty,
      localNamespace = S.empty
    }

initBuiltins :: TypeCheckMonad ()
initBuiltins = do
  let builtins = [PrintInt, PrintString, ThrowError, ReadInt, ReadString]
  let addBuiltinToEnv builtin = modify $ \state -> state {functions = M.insert (ident builtin) (builtinType builtin) (functions state)}
    in do
      mapM_ addBuiltinToEnv builtins

runTypeCheck :: Program -> ExceptT String IO ()
runTypeCheck program = do
  let res = evalStateT (runExceptT $ withExceptT showError $ checkTypes program) emptyState
   in ExceptT res


addVarToEnv :: Stmt -> String -> LangType -> TypeCheckMonad ()
addVarToEnv stmt name t = do
  when (t == VoidType) $ throwError $ IllegalType (hasPosition stmt) (show t)
  lns <- gets localNamespace
  if S.member name lns
    then throwError $ VariableAlreadyDefined stmt name
    else modify $ \state -> state {
      variables = M.insert name t (variables state),
      localNamespace = S.insert name lns
      }

addParameterToEnv :: Arg -> String -> LangType -> TypeCheckMonad ()
addParameterToEnv arg name t = do
  when (t == VoidType) $ throwError $ IllegalType (hasPosition arg) (show t)
  lns <- gets localNamespace
  if S.member name lns
    then throwError $ ParameterAlreadyDefined arg name
    else modify $ \state -> state {
      variables = M.insert name t (variables state),
      localNamespace = S.insert name lns
      }

getVariableType :: BNFC'Position -> String -> TypeCheckMonad LangType
getVariableType pos name = do
  varEnv <- gets variables
  case M.lookup name varEnv of
    Just t -> return t
    Nothing -> throwError $ VariableNotFound pos name

getFunctionType :: Expr -> String -> TypeCheckMonad FunctionType
getFunctionType expr name = do
  funEnv <- gets functions
  case M.lookup name funEnv of
    Just t -> return t
    Nothing -> throwError $ FunctionNotFound expr name

addFunctionToEnv :: TopDef -> String -> FunctionType -> TypeCheckMonad ()
addFunctionToEnv pos name funType = do
  funs <- gets functions
  if M.member name funs
    then throwError $ FunctionAlreadyDefined pos name
    else modify $ \state -> state {functions = M.insert name funType (functions state)}

addClassToEnv :: TopDef -> String -> ClassTypeDef -> TypeCheckMonad ()
addClassToEnv def name classType = do
  _classes <- gets classes
  if M.member name _classes
    then throwError $ ClassAlreadyDefined def name
    else modify $ \state -> state {classes = M.insert name classType _classes}

-- function that gets a function that modifies the environment
-- and runs the computation with the modified environment
withLocalModifiedEnv :: TypeCheckMonad () -> TypeCheckMonad a -> TypeCheckMonad a
withLocalModifiedEnv modifyEnv action = do
  oldEnv <- get
  modify $ \state -> state {localNamespace = S.empty}
  modifyEnv
  result <- action
  put oldEnv
  return result

withLocalEnv :: TypeCheckMonad a -> TypeCheckMonad a
withLocalEnv action = do
  oldEnv <- get
  modify $ \state -> state {localNamespace = S.empty}
  result <- action
  put oldEnv
  return result

------------------ TypeCheckable instances ------------------

class TypeCheckable a where
  inferType :: a -> TypeCheckMonad LangType
  inferType _ = return VoidType

  checkTypes :: a -> TypeCheckMonad ()
  checkTypes _ = return ()


instance TypeCheckable Program where
  checkTypes :: Program -> TypeCheckMonad ()
  checkTypes (Program _ defs) = do
    let isFunctionDef (FnDef _ _ _ _ _) = True
        isFunctionDef _ = False

        fnDefs = filter isFunctionDef defs
        clsDefs = filter (not . isFunctionDef) defs
        expectedMainType = FunctionType IntType []
        getFunctionName (FnDef _ _ ident _ _) = getIdent ident
        getClassName (ClsDef _ ident _) = getIdent ident
    
    initBuiltins
    mapM_ (\def -> addFunctionToEnv def (getFunctionName def) (functionFromAst def)) fnDefs
    mapM_ (\def -> addClassToEnv def (getClassName def) (classFromAst def)) clsDefs
    checkForMain
    mapM_ checkTypes defs
    where 
      expectedMainType = FunctionType IntType []
      checkForMain :: TypeCheckMonad ()
      checkForMain = do
        declaredFunctions <- gets functions
        let mainType = M.lookup "main" declaredFunctions
        case mainType of
          Nothing -> throwError MainNotPresent
          Just t -> when (t /= expectedMainType) $ throwError $ MainWrongType (show t) (show expectedMainType)

instance TypeCheckable TopDef where
  -- This is currently unused
  inferType :: TopDef -> TypeCheckMonad LangType
  inferType (FnDef _ retType _ args _) = do
    _argTypes <- mapM inferType args
    _retType <- inferType retType
    let funType = FunType _retType _argTypes
    return funType

  inferType (ClsDef _ ident items) = do
    return $ ClassType (getIdent ident)

  checkTypes (FnDef _ retType name args block) = do
    _argTypes <- mapM inferType args
    _retType <- inferType retType
    let funType = FunctionType _retType _argTypes
    let modifyEnv = do
          modify $ \state -> state {currentFunctionType = funType}
          mapM_ addArgumentToEnv args
    withLocalModifiedEnv modifyEnv $ checkTypes block
    where 
      addArgumentToEnv :: Arg -> TypeCheckMonad ()
      addArgumentToEnv arg@(Arg _ t ident) = do
        ty <- inferType t
        addParameterToEnv arg (getIdent ident) ty


  checkTypes (ClsDef _ _ items) = do
    return ()

instance TypeCheckable Block where
  checkTypes (Block _ stmts) = do
    withLocalEnv (mapM_ checkTypes stmts)

instance TypeCheckable Stmt where
  checkTypes (Empty _) = return ()
  checkTypes (BStmt _ block) = checkTypes block
  checkTypes stmt@(Decl _ t items) = do
    _t <- inferType t
    let addVar expected (NoInit _ ident) = do
          addVarToEnv stmt (getIdent ident) expected
        addVar expected (Init _ ident expr) = do
          exprType <- inferType expr
          if exprType /= expected
            then throwError $ UnexpectedExpressionType expr (show exprType) (show expected)
            else addVarToEnv stmt (getIdent ident) expected
    mapM_ (addVar _t) items
  checkTypes (Ass pos lvalue expr) = do
    exprType <- inferType expr
    varType <- inferType lvalue
    if exprType /= varType
      then throwError $ UnexpectedExpressionType expr (show exprType) (show varType)
      else return ()
  checkTypes stmt@(Incr pos lvalue) = do
    varType <- inferType lvalue
    if varType /= IntType
      then throwError $ UnexpectedVariableType stmt (printTree lvalue) (show varType) (show IntType)
      else return ()
  checkTypes stmt@(Decr pos lvalue) = do
    varType <- inferType lvalue
    if varType /= IntType
      then throwError $ UnexpectedVariableType stmt (printTree lvalue) (show varType) (show IntType)
      else return ()
  checkTypes stmt@(Ret _ expr) = do
    exprType <- inferType expr
    funcType <- gets currentFunctionType
    let retVal = returns funcType
    if exprType /= retVal
      then throwError $ WrongReturnType stmt (show exprType) (show retVal)
      else return ()
  checkTypes stmt@(VRet _) = do
    funcType <- gets currentFunctionType
    let retVal = returns funcType
    if retVal /= VoidType
      then throwError $ WrongReturnType stmt (show VoidType) (show retVal)
      else return ()
  checkTypes (Cond _ expr stmt) = do
    exprType <- inferType expr
    if exprType /= BoolType
      then throwError $ UnexpectedExpressionType  expr (show exprType) (show BoolType)
      else do
        checkStmtForIllegalDeclaration stmt
        checkTypes stmt
  checkTypes (CondElse _ expr stmt1 stmt2) = do
    exprType <- inferType expr
    if exprType /= BoolType
      then throwError $ UnexpectedExpressionType  expr (show exprType) (show BoolType)
      else do
        checkStmtForIllegalDeclaration stmt1
        checkTypes stmt1
        checkStmtForIllegalDeclaration stmt2
        checkTypes stmt2
  checkTypes (While _ expr stmt) = do
    exprType <- inferType expr
    if exprType /= BoolType
      then throwError $ UnexpectedExpressionType  expr (show exprType) (show BoolType)
      else do
        checkStmtForIllegalDeclaration stmt
        checkTypes stmt
  checkTypes (SExp _ expr) = do
    checkTypes expr
    return ()
  checkTypes stmt@(For _ t ident expr block) = return () -- @TODO

checkBinaryOperationTypes ::  LangType -> Expr -> Expr -> TypeCheckMonad ()
checkBinaryOperationTypes expectedType expr1 expr2 = do
  exprType1 <- inferType expr1
  exprType2 <- inferType expr2
  if exprType1 /= expectedType
    then throwError $ UnexpectedExpressionType  expr1 (show exprType1) (show expectedType)
    else
      if exprType2 /= expectedType
        then throwError $ UnexpectedExpressionType  expr2 (show exprType2) (show expectedType)
        else return ()

checkUnaryOperationTypes :: LangType -> Expr -> TypeCheckMonad ()
checkUnaryOperationTypes expectedType expr = do
  exprType <- inferType expr
  if exprType /= expectedType
    then throwError $ UnexpectedExpressionType expr (show exprType) (show expectedType)
    else return ()

instance TypeCheckable Expr where
  inferType (ELValue pos lvalue) = inferType lvalue
  inferType expr@(ELitInt _ value) = do
    if value > 2^31 - 1 || value < - (2 ^ 31)
      then throwError $ IntegerOverflow expr
      else return IntType
  inferType (ELitTrue _) = return BoolType
  inferType (ELitFalse _) = return BoolType
  inferType expr@(EApp _ ident exprs) = do
    funType <- getFunctionType expr (getIdent ident)
    argTypes <- mapM inferType exprs
    let funArgTypes = args funType
    if length argTypes /= length funArgTypes
      then throwError $ WrongNumberOfArguments expr (getIdent ident) (length argTypes) (length funArgTypes)
      else do
        mapM_ checkArgType (zip3 exprs funArgTypes argTypes)
        return $ returns funType
        where
            checkArgType::(Expr, LangType, LangType)->TypeCheckMonad ()
            checkArgType (expr, expected, actual) =
              if expected /= actual
                then throwError $ UnexpectedExpressionType expr (show actual) (show expected)
                else return ()
  inferType (EString _ _) = return StringType
  inferType (Neg _ expr) = do
    checkUnaryOperationTypes  IntType expr
    return IntType
  inferType (Not _ expr) = do
    checkUnaryOperationTypes  BoolType expr
    return BoolType
  inferType (EMul _ expr1 _ expr2) = do
    checkBinaryOperationTypes  IntType expr1 expr2
    return IntType
  inferType expr@(EAdd _ expr1 op expr2) = do
    case op of
      Plus _ -> do
        exprType1 <- inferType expr1
        exprType2 <- inferType expr2
        case exprType1 of
          IntType -> checkBinaryOperationTypes  IntType expr1 expr2
          StringType -> checkBinaryOperationTypes  StringType expr1 expr2
          _ -> throwError $ WrongTypeForOperator expr "+" (show exprType1) (show exprType2)
        return exprType1
      _ -> do
        checkBinaryOperationTypes  IntType expr1 expr2
        return IntType
  inferType expr@(ERel _ expr1 op expr2) = do
    case op of
      EQU _ -> do
        handleEquality
      NE _ -> do
        handleEquality
      _ -> do
        handleIntegerComparison
    where
      handleEquality = do
        exprType1 <- inferType expr1
        exprType2 <- inferType expr2
        case exprType1 of
          IntType -> do
            checkBinaryOperationTypes  IntType expr1 expr2
            return BoolType
          BoolType -> do
            checkBinaryOperationTypes  BoolType expr1 expr2
            return BoolType
          StringType -> do
            checkBinaryOperationTypes  StringType expr1 expr2
            return BoolType
          _ ->
            if exprType1 == exprType2
              then return BoolType
              else throwError $ WrongTypesForOperator expr (show op) (show exprType1) (show exprType2)
      handleIntegerComparison = do
        checkBinaryOperationTypes  IntType expr1 expr2
        return BoolType
  inferType (EAnd _ expr1 expr2) = do
    checkBinaryOperationTypes  BoolType expr1 expr2
    return BoolType
  inferType (EOr _ expr1 expr2) = do
    checkBinaryOperationTypes  BoolType expr1 expr2
    return BoolType
  inferType (ENewObject _ ident) = return $ ClassType (getIdent ident)
  inferType (ENewArray _ t expr) = do
    exprType <- inferType expr
    if exprType /= IntType
      then throwError $ UnexpectedExpressionType expr (show exprType) (show IntType)
      else return $ ArrayType (fromAstType t)
  inferType (ENull _ t) = return $ ClassType (getIdent t)

  checkTypes expr = inferType expr >> return ()

instance TypeCheckable LValue where
  inferType (LVar pos ident) = getVariableType pos (getIdent ident)
  inferType (LArr pos lvalue expr) = do
    exprType <- inferType expr
    if exprType /= IntType
      then throwError $ UnexpectedExpressionType expr (show exprType) (show IntType)
      else do
        varType <- inferType lvalue
        case varType of
          ArrayType t -> return t
          _ -> throwError $ UnexpectedExpressionType (ELValue pos (LArr pos lvalue expr)) (show varType) "Array"
  inferType (LAttr pos lvalue attr) = do
    varType <- inferType lvalue
    case varType of
      ClassType className -> do
        classDef <- gets (M.lookup className . classes)
        case classDef of 
          Just classType ->
            case getClassType classType (getIdent attr) of
              Just t -> return t
              Nothing -> throwError $ AttributeNotFound pos (printTree lvalue) (getIdent attr) 
          Nothing -> throwError $ ClassNotFound pos className
      ArrayType _ -> do
        case getIdent attr of
          "length" -> return IntType
          _ -> throwError $ AttributeNotFound pos (printTree lvalue) (getIdent attr)
      _ -> throwError $ UnexpectedExpressionType (ELValue pos (LAttr pos lvalue attr)) (show varType) "Class"

instance TypeCheckable Type where
  inferType t =return $ fromAstType t
-- Helper functions

instance TypeCheckable Arg where
  inferType (Arg _ t _) = inferType t

checkStmtForIllegalDeclaration :: Stmt -> TypeCheckMonad ()
checkStmtForIllegalDeclaration stmt@(Decl _ _ _) = throwError $ ConditionalDeclaration stmt
checkStmtForIllegalDeclaration _ = return ()