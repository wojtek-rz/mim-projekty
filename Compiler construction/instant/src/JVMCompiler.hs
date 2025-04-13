module JVMCompiler (compile) where
import Bnfc.Abs
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utilities (parseCode, writeToFile, runCommandEcho, printPos, maxFromZero)
import System.FilePath (takeBaseName, takeDirectory, replaceExtension)
import GHC.IO.Exception (ExitCode(ExitSuccess))
import Bnfc.Print (printTree)

type VarName = String
type LocalsStackId = Int

type Bytecode = String
type LocalsLimit = Int -- local variables stack
type StackLimit = Int -- operand stack

type LocalsStack = Map.Map VarName LocalsStackId
type CompileM result = ExceptT String (ReaderT LocalsStack IO) result


--------------------- EMIT FUNCTIONS ---------------------

emitConst :: Int -> Bytecode
emitConst n
    | n == -1 = "iconst_m1\n"
    | n >= 0 && n <= 5 = "iconst_" ++ show n ++ "\n"
    | n >= -128 && n <= 127 = "bipush " ++ show n ++ "\n"
    | n >= -32768 && n <= 32767 = "sipush " ++ show n ++ "\n"
    | otherwise = "ldc " ++ show n ++ "\n"

emitLoad :: Int -> Bytecode
emitLoad n
    | n <= 3 = "iload_" ++ show n ++ "\n"
    | otherwise = "iload " ++ show n ++ "\n"

emitStore :: Int -> Bytecode
emitStore n
    | n <= 3 = "istore_" ++ show n ++ "\n"
    | otherwise = "istore " ++ show n ++ "\n"

-- Pop the top of the stack and print it
emitPrint :: Bytecode
emitPrint = "getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
            emitSwap ++ "\n" ++
            "invokevirtual java/io/PrintStream/println(I)V\n"

emitAdd :: Bytecode
emitAdd = "iadd\n"

emitMult :: Bytecode
emitMult = "imul\n"

emitSub :: Bytecode
emitSub = "isub\n"

emitDiv :: Bytecode
emitDiv = "idiv\n"

emitSwap :: Bytecode
emitSwap = "swap\n"

emitFileBytecode :: String -> StackLimit -> LocalsLimit -> Bytecode -> String
emitFileBytecode name stackLimit localsLimit bc =
    ".class public " ++ name ++ "\n" ++
    ".super java/lang/Object\n\n" ++
    ".method public <init>()V\n" ++
    "    aload_0\n" ++
    "    invokespecial java/lang/Object/<init>()V\n" ++
    "    return\n" ++
    ".end method\n\n" ++
    ".method public static main([Ljava/lang/String;)V\n" ++
    ".limit locals " ++ show localsLimit ++ "\n" ++  -- Użycie limitu zmiennych lokalnych
    ".limit stack " ++ show stackLimit ++ "\n" ++
    bc ++ "\n" ++
    "return\n" ++
    ".end method\n"

--------------------- COMPILATION FUNCTIONS ---------------------


data Order = Swapped | Same deriving (Eq)

_exprOptimalBytecode :: Exp -> Exp -> CompileM (Bytecode, Int, Order)
_exprOptimalBytecode expr1 expr2 = do
    (bc1, stackLimit1) <- compileExpr expr1
    (bc2, stackLimit2) <- compileExpr expr2
    if stackLimit1 < stackLimit2
        then return (bc2 ++ bc1, stackLimit1 + 1, Swapped)
        else return (bc1 ++ bc2, stackLimit2 + 1, Same)

compileExpr :: Exp -> CompileM (Bytecode, StackLimit)
compileExpr expr@(ExpLit pos n) = do 
    if n < 0
        then throwError $ "Negative numbers are not supported\n" ++ printTree expr ++ printPos pos
        else return (emitConst $ fromInteger n, 1)

compileExpr (ExpVar pos (Ident varName)) = do
    localsStack <- ask
    case Map.lookup varName localsStack of
        Nothing -> throwError $ "Variable " ++ varName ++ " not found\n" ++ printPos pos
        Just varId -> return (emitLoad varId, 1)

compileExpr (ExpAdd _ expr1 expr2) = do
    (bc, stackLimit, _) <- _exprOptimalBytecode expr1 expr2
    return (bc ++ emitAdd, stackLimit)

compileExpr (ExpSub _ expr1 expr2) = do
    (bc, stackLimit, swapped) <- _exprOptimalBytecode expr1 expr2
    bc2 <- if swapped == Swapped then return $ bc ++ emitSwap else return bc
    return (bc2 ++ emitSub, stackLimit)

compileExpr (ExpMul _ expr1 expr2) = do
    (bc, stackLimit, _) <- _exprOptimalBytecode expr1 expr2
    return (bc ++ emitMult, stackLimit)

compileExpr (ExpDiv _ expr1 expr2) = do
    (bc, stackLimit, swapped) <- _exprOptimalBytecode expr1 expr2
    bc2 <- if swapped == Swapped then return $ bc ++ emitSwap else return bc
    return (bc2 ++ emitDiv, stackLimit)

compileStmt :: Stmt -> CompileM (Bytecode, StackLimit)
compileStmt (SExp _ expr) = do
    (bc, stackLimit) <- compileExpr expr
    return (bc ++ emitPrint, stackLimit + 1)

compileStmt (SAss _ (Ident varName) expr) = do
    localsStack <- ask
    case Map.lookup varName localsStack of
        Nothing -> throwError $ "Variable " ++ varName ++ " not found"
        Just varId -> do
            (bc, stackLimit) <- compileExpr expr
            return (bc ++ emitStore varId, max stackLimit 2)

compileProgram :: Program -> CompileM (Bytecode, StackLimit)
compileProgram (Prog _ stmts) = do
    results <- mapM compileStmt stmts
    (bytecodes, limits) <- return $ unzip results
    return (concat bytecodes, maxFromZero limits)


--------------------- HIGHER COMPILITATION FUNCTIONS ---------------------

getLocalsStack :: Program -> (LocalsStack, LocalsLimit)
getLocalsStack (Prog _ stmts) =
    let varStack = Map.fromList $ zip (removeDuplicates $ getVariables stmts) [0..]
    in (varStack, 1 + Map.size varStack)
    where
        getVariables [] = []
        getVariables (SAss _ (Ident varName) _ : rest) = varName : getVariables rest
        getVariables (SExp _ _ : rest) = getVariables rest
        removeDuplicates = Set.toList . Set.fromList

runCompileProgram :: Program -> LocalsStack -> ExceptT String IO (Bytecode, StackLimit)
runCompileProgram program localsStack = 
    let bc = runReaderT (runExceptT (compileProgram program)) localsStack
        in ExceptT bc

getBytecode :: String -> String -> ExceptT String IO String
getBytecode name code = do
    pst <- parseCode code
    (localsStack, localsLimit) <- return $ getLocalsStack pst
    (bc, stackLimit) <- runCompileProgram pst localsStack
    return $ emitFileBytecode name stackLimit localsLimit bc

_compile :: String -> ExceptT String IO ()
_compile filePath = do
    let name = takeBaseName filePath
        jasminPath = replaceExtension filePath ".j"
        classPath = replaceExtension filePath ".class"
    contents <- liftIO $ readFile filePath
    bytecode <- getBytecode name contents
    liftIO $ writeToFile jasminPath bytecode
    exitCode <- liftIO $ runCommandEcho $ "java -jar lib/jasmin.jar " ++ "-d " ++ takeDirectory filePath ++ " " ++ jasminPath
    if exitCode /= ExitSuccess
        then throwError "Jasmin compilation failed. "
        else liftIO $ putStrLn $ "Compiled binary is located in " ++ classPath

compile :: String -> IO ()
compile filePath = do
    result <- runExceptT $ _compile filePath
    case result of
        Left err -> putStrLn err
        Right _ -> return ()
