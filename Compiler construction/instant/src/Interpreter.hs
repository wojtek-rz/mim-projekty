module Interpreter (interpret) where

import Bnfc.Abs
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Bnfc.Par ( myLexer, pProgram )
import Bnfc.Lex ( mkPosToken, Token )

type Name = String
type Value = Integer

type Env = Map.Map Name Value
type EvalM result = ExceptT String (StateT Env IO) result

entryEnv :: Map.Map k a
entryEnv = Map.empty

evalExpr :: Exp -> EvalM Integer
evalExpr (ExpLit _ n) = return n
evalExpr (ExpVar _ (Ident name)) = do
    env <- get
    case Map.lookup name env of
        Just val -> return val
        Nothing -> throwError $ "Variable " ++ name 
evalExpr (ExpAdd _ e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    return $ v1 + v2
evalExpr (ExpSub _ e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    return $ v1 - v2
evalExpr (ExpMul _ e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    return $ v1 * v2
evalExpr (ExpDiv _ e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    if v2 == 0 then throwError "Division by zero" else return $ v1 `div` v2

evalStmt :: Stmt -> EvalM ()
evalStmt (SExp _ expr) = do
    value <- evalExpr expr
    liftIO $ print value
    return ()

evalStmt (SAss _ (Ident name) expr) = do
    value <- evalExpr expr
    modify $ Map.insert name value
    return ()


evalProgram :: Program -> EvalM ()
evalProgram (Prog _ stmts) = do
    mapM_ evalStmt stmts

runEval :: Program -> IO (Either String ())
runEval program = evalStateT (runExceptT  (evalProgram program)) entryEnv

showPosToken :: Token -> [Char]
showPosToken token =
            let ((l,c),t) = mkPosToken token in
            concat [ show l, ":", show c, "\t", show t ]

interpret :: String -> IO ()
interpret code = do
    let tokens = myLexer code
        parseTree = pProgram tokens
    case parseTree of
        Left err ->
            putStrLn $ concat ["Parse failed: \n", showPosToken $ last tokens, "\n", err]
        Right tree -> do
            res <- runEval tree
            case res of
                Left err -> putStrLn $ "Runtime error: \n" ++ err
                Right _ -> putStrLn "Program executed successfully"