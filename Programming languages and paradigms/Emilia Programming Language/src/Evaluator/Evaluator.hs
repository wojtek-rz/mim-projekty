module Evaluator.Evaluator (runEval, runEvalRepl) where
import Bnfc.Abs
import Evaluator.LangData
import Evaluator.Exceptions
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Evaluator.Builtins

evalArg :: Arg -> Expr -> EvalM ArgValue
evalArg (FunArg _ _ (ValueArgType _ _)) expr = do
    val <- evalExpr expr
    return $ Value val

evalArg (FunArg pos (Ident name) (RefArgType _ _)) expr = do
    case expr of
        EVar _ (Ident refName) -> do
            loc <- getVarLoc refName
            return $ Ref loc
        _ -> throwError $ InternalError ( name ++ " argument is not a variable.") (Position pos)

evalArgs :: [Arg] -> [Expr] -> EvalM [ArgValue]
evalArgs args exprs = do
    zipWithM evalArg args exprs

evalExpr :: Expr -> EvalM Value
evalExpr (EVar _ (Ident name)) = getVarValue name

evalExpr (ELitInt _ i) = return $ IntValue i

evalExpr (ELitTrue _) = return $ BoolValue True

evalExpr (ELitFalse _) = return $ BoolValue False

evalExpr (ELambda _ args retType block) = do
    let lambdaFunc = evalBlock block >> setIgnoreStmts False >> getReturnValue
        in do
            envValue <- gets env
            return $ LambdaValue args retType lambdaFunc envValue

evalExpr (ELambdaShort _ args retType expr) = do
    let lambdaFunc = evalExpr expr
        in do
            envValue <- gets env
            return $ LambdaValue args retType lambdaFunc envValue

evalExpr topExpr@(EApp _ (Ident name) exprs) = do
    function <- getVarValue name
    setReturnValue VoidValue
    case function of
        LambdaValue args _ lambdaFunc lambdaEnv -> do
            argsValues <- evalArgs args exprs
            withRestoredEnv (
                setEnv lambdaEnv >>
                zipWithM addArg args argsValues >>
                lambdaFunc
                )
        _ -> throwError $ InternalError ("Variable " ++ name ++ " is not a function.") (ExprFragment topExpr)

evalExpr (EString _ s) = return $ StringValue s

evalExpr (Concat _ e1 e2) = do
    StringValue s1 <- evalExpr e1
    StringValue s2 <- evalExpr e2
    return $ StringValue $ s1 ++ s2

evalExpr (Neg _ e) = do
    IntValue i <- evalExpr e
    return $ IntValue (-i)

evalExpr (Not _ e) = do
    BoolValue b <- evalExpr e
    return $ BoolValue $ not b

evalExpr (EMul _ e1 op e2) = do
    IntValue i1 <- evalExpr e1
    IntValue i2 <- evalExpr e2
    case op of
        Times _ -> return $ IntValue $ i1 * i2
        Div _ -> if i2 == 0 then throwError $ DivisionByZeroException e2
                 else return $ IntValue $ i1 `div` i2
        Mod _ -> if i2 == 0 then throwError $ DivisionByZeroException e2
                 else return $ IntValue $ i1 `mod` i2

evalExpr (EAdd _ e1 op e2) = do
    IntValue i1 <- evalExpr e1
    IntValue i2 <- evalExpr e2
    case op of
        Plus _ -> return $ IntValue $ i1 + i2
        Minus _ -> return $ IntValue $ i1 - i2

evalExpr (ERel _ e1 op e2) = do
    IntValue i1 <- evalExpr e1
    IntValue i2 <- evalExpr e2
    case op of
        LTH _ -> return $ BoolValue $ i1 < i2
        LE _ -> return $ BoolValue $ i1 <= i2
        GTH _ -> return $ BoolValue $ i1 > i2
        GE _ -> return $ BoolValue $ i1 >= i2

evalExpr (EEquals _ e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (LambdaValue {}, LambdaValue {}) ->
            return $ BoolValue False
        _ -> return $ BoolValue $ v1 == v2

evalExpr (ENequals _ e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    return $ BoolValue $ v1 /= v2

evalExpr (EAnd _ e1 e2) = do
    BoolValue b1 <- evalExpr e1
    (if b1
        then (do
            BoolValue b2 <- evalExpr e2
            return $ BoolValue b2)
        else return $ BoolValue False)

evalExpr (EOr _ e1 e2) = do
    BoolValue b1 <- evalExpr e1
    (if b1
        then return $ BoolValue True
        else (do
            BoolValue b2 <- evalExpr e2
            return $ BoolValue b2))

evalStmt :: Stmt -> EvalM Value
evalStmt stmt@(Decl _ (Ident name) expr) = do
    value <- evalExpr expr
    addVar name value
    case expr of
        ELambda {} ->  do -- Fix to add recurence, because from expr we don't have the name of the function.
            case value of
                LambdaValue args retType lambdaFunc _ -> do
                    lambdaEnvWithFunc <- gets env
                    setVarValue name (LambdaValue args retType lambdaFunc lambdaEnvWithFunc)
                _ -> throwError $ InternalError "Lambda expression is not a lambda value." (StmtFragment stmt)

            return value
        _ -> return value

evalStmt (Pass _) = return $ IntValue 0

evalStmt (Assign _ (Ident name) expr) = do
    value <- evalExpr expr
    setVarValue name value
    return value

evalStmt (Cond _ expr block) = do
    BoolValue b <- evalExpr expr
    if b then evalBlock block
    else return $ IntValue 0

evalStmt (CondElse _ expr block1 block2) = do
    BoolValue b <- evalExpr expr
    if b then evalBlock block1
    else evalBlock block2

evalStmt (While _ expr block) = do
    BoolValue b <- evalExpr expr
    if b then evalBlock block >> evalStmt (While Nothing expr block)
    else return $ IntValue 0

evalStmt (Ret _ expr) = do
    value <- evalExpr expr
    setReturnValue value
    setIgnoreStmts True
    return value

evalStmt (VRet _) = do
    setReturnValue VoidValue
    setIgnoreStmts True
    return VoidValue

evalStmt (ExprStmt _ epxr)  = evalExpr epxr

evalOrIgnoreStmt :: Stmt -> EvalM Value
evalOrIgnoreStmt stmt = do
    ignore <- gets ignoreStmts
    if ignore then return $ IntValue 0
    else evalStmt stmt


evalBlock :: Block -> EvalM Value
evalBlock (StmtBlock _ stmts) = do
    if null stmts
    then return VoidValue
    else do
        results <- mapM evalOrIgnoreStmt stmts
        return $ last results

evalProgram :: Program -> EvalM Value
evalProgram (ProgramMain _ stmts) = evalBlock (StmtBlock Nothing stmts)

printException :: EvalException -> String
printException exception =
    "Runtime exception: \n" ++ show exception

runEval :: Program -> ExceptT String IO ()
runEval program =
    let runProgram = evalProgram program >> return ()
        res = evalStateT (runExceptT (withExceptT printException $ runProgram)) entryState
        in ExceptT res

runEvalRepl :: EvalState -> Program -> IO (Either String (String, EvalState))
runEvalRepl startState program =
    let runProgram = evalProgram program
        result = runStateT (runExceptT (withExceptT printException runProgram)) startState
        in do
            (res, newState) <- result
            case res of
                Left errorMsg -> return $ Left errorMsg
                Right value -> return $ Right (show value, newState)
