module TypeChecker.TypeChecker (checkTypes, checkTypesRepl) where
import TypeChecker.Exceptions
import TypeChecker.LangTypes
import TypeChecker.Utils
import TypeChecker.Monad
import Bnfc.Abs
import Control.Monad
import Control.Monad.State
import Control.Monad.Except


checkExprSuitableToArg :: Expr -> LangArgType -> TypeCheckM ()
checkExprSuitableToArg expr argType = do
    exprType <- evalExprType expr
    expectedType <- return $ _toType argType
    checkLangTypesEqual expectedType exprType expr
    case argType of
        ValueArg _ -> return ()
        RefArg _ -> case expr of
            EVar _ _ -> return ()
            _ -> throwError $ RefArgumentIsNotVariableException expr

evalExprType :: Expr -> TypeCheckM LangType
evalExprType (EVar pos (Ident name)) = do
    getVarType name (VariableNotDeclaredException name pos)

evalExprType (ELitInt _ _ ) = return IntType

evalExprType (ELitTrue _) = return BoolType

evalExprType (ELitFalse _) = return BoolType

evalExprType (ELambdaShort _ args retType expr) = do
    exprType <- withRestoredState (addArgsToState args >>
                                   evalExprType expr)
    let retLangType = getAnnotationType retType
    checkLangTypesEqual retLangType exprType expr
    return $ getLambdaType args retType

evalExprType (ELambda pos args retType block) = do
    withRestoredState (addArgsToState args >>
                       setReturnValue (getAnnotationType retType) >>
                       checkBlockTypes block >>
                       checkReturnOccurred pos)
    return $ getLambdaType args retType

evalExprType (EApp pos (Ident name) exprs) = do
    funcType <- getVarType name (VariableNotDeclaredException name pos)
    case funcType of
        FunType argsTypes retType -> do
            let exprsLenght = length exprs
                argsLenght = length argsTypes
            if exprsLenght /= argsLenght
            then throwError $ WrongNumberOfArgumentsException argsLenght exprsLenght (EApp pos (Ident name) exprs)
            else mapM_ (uncurry checkExprSuitableToArg) (zip exprs argsTypes) >> return retType
        _ -> throwError $ NotAFucntionException funcType name (EApp pos (Ident name) exprs)

evalExprType (EString _ _) = return StringType

evalExprType (Concat _ e1 e2) = do
    t1 <- evalExprType e1
    t2 <- evalExprType e2
    checkLangTypesEqual StringType t1 e1
    checkLangTypesEqual StringType t2 e2
    return StringType

evalExprType (Neg _ e) = do
    t <- evalExprType e
    checkLangTypesEqual IntType t e
    return IntType

evalExprType (Not _ e) = do
    t <- evalExprType e
    checkLangTypesEqual BoolType t e
    return BoolType

evalExprType (EMul _ e1 _ e2) = do
    t1 <- evalExprType e1
    t2 <- evalExprType e2
    checkLangTypesEqual IntType t1 e1
    checkLangTypesEqual IntType t2 e1
    return IntType

evalExprType (EAdd _ e1 _ e2) = do
    t1 <- evalExprType e1
    t2 <- evalExprType e2
    checkLangTypesEqual IntType t1 e1
    checkLangTypesEqual IntType t2 e2
    return IntType

evalExprType (ERel _ e1 _ e2) = do
    t1 <- evalExprType e1
    t2 <- evalExprType e2
    checkLangTypesEqual IntType t1 e1
    checkLangTypesEqual IntType t2 e2
    return BoolType

evalExprType (EEquals _ e1 e2) = do
    t1 <- evalExprType e1
    t2 <- evalExprType e2
    checkLangTypesEqual t1 t2 e2
    return BoolType

evalExprType (ENequals _ e1 e2) = do
    t1 <- evalExprType e1
    t2 <- evalExprType e2
    checkLangTypesEqual t1 t2 e2
    return BoolType

evalExprType (EAnd _ e1 e2) = do
    t1 <- evalExprType e1
    t2 <- evalExprType e2
    checkLangTypesEqual BoolType t1 e1
    checkLangTypesEqual BoolType t2 e2
    return BoolType

evalExprType (EOr _ e1 e2) = do
    t1 <- evalExprType e1
    t2 <- evalExprType e2
    checkLangTypesEqual BoolType t1 e1
    checkLangTypesEqual BoolType t2 e2
    return BoolType

checkStmtTypes :: Stmt -> TypeCheckM ()
checkStmtTypes (Decl _ (Ident name) expr) = do
    declType <- case expr of
        ELambda _ args retType _ -> do
            let declType = getLambdaType args retType -- recurence functions fix
            withRestoredState (addVarType name declType >>
                               evalExprType expr >> return ())
            return declType
        _ -> evalExprType expr

    case declType of
        VoidType -> throwError $ VoidTypeDeclarationException name expr
        _ -> return ()

    addVarType name declType

checkStmtTypes (Pass _) = return ()

checkStmtTypes (Assign pos (Ident name) expr) = do
    t <- evalExprType expr
    varType <- getVarType name (VariableNotDeclaredException name pos)
    checkLangTypesEqual varType t expr

checkStmtTypes (Cond _ expr block) = do
    t <- evalExprType expr
    checkLangTypesEqual BoolType t expr
    withRestoredState (checkBlockTypes block)

checkStmtTypes (CondElse _ expr block1 block2) = do
    t <- evalExprType expr
    checkLangTypesEqual BoolType t expr
    occurred1 <- withRestoredState (
        checkBlockTypes block1 >>
        checkReturnOccurred (hasPosition block1) >>
        gets returnOccurred)
    occurred2 <- withRestoredState (
        checkBlockTypes block2 >>
        checkReturnOccurred (hasPosition block2) >>
        gets returnOccurred)
    when (occurred1 && occurred2) setReturnOccurred

checkStmtTypes (While _ expr block) = do
    t <- evalExprType expr
    checkLangTypesEqual BoolType t expr
    withRestoredState (checkBlockTypes block)

checkStmtTypes (ExprStmt _ expr) = do
    _ <- evalExprType expr
    return ()

checkStmtTypes (Ret _ expr) = do
    t <- evalExprType expr
    currentReturnType <- gets returnType
    setReturnOccurred
    checkLangTypesEqual currentReturnType t expr

checkStmtTypes (VRet pos) = do
    currentReturnType <- gets returnType
    setReturnOccurred
    if currentReturnType == VoidType then return ()
    else throwError $ FunctionNotVoidReturnTypeException currentReturnType VoidType pos

checkProgramTypes :: Program -> TypeCheckM ()
checkProgramTypes (ProgramMain _ stmts) = do
    mapM_ checkStmtTypes stmts

checkBlockTypes :: Block -> TypeCheckM ()
checkBlockTypes (StmtBlock _ stmts) = do
    mapM_ checkStmtTypes stmts

printException :: TypeCheckException -> String
printException exception =
    "Type checking failed: \n" ++ show exception


checkTypes :: Program -> ExceptT String IO ()
checkTypes program = do
    let res = evalStateT (runExceptT (withExceptT printException $ checkProgramTypes program)) entryTypeState
        in ExceptT res

checkTypesRepl :: TypeState -> Program -> IO (Either String TypeState)
checkTypesRepl replState program =
    let result = runStateT (runExceptT (withExceptT printException $ checkProgramTypes program)) replState
        in do
            (res, newState) <- result
            case res of
                Left err -> return $ Left err
                Right _ -> return $ Right newState