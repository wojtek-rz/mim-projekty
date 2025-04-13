module LLVMCompiler(compile) where
import Control.Monad.Except
import Utilities
import qualified Data.Map as Map
import Control.Monad.State
import Bnfc.Abs
import System.FilePath
import GHC.IO.Exception (ExitCode(ExitSuccess))
import qualified Data.Set as Set


type Bytecode = String
data Reg = Const Int | Reg Int
type VarName = String
type VariablesDict = Map.Map VarName Reg

type Env = (VariablesDict, Int) -- (variables, next available register)
type CompileM result = ExceptT String (StateT Env IO) result

getNewReg :: CompileM Reg
getNewReg = do
    (vars, regIdx) <- get
    put (vars, regIdx + 1)
    return $ Reg regIdx

getVariableReg :: VarName -> CompileM (Maybe Reg)
getVariableReg varname = do
    (vars, _) <- get
    return $ Map.lookup varname vars

emitReg :: Reg -> Bytecode
emitReg (Reg int) = "%r" ++ show int
emitReg (Const int) = show int

emitAlloc :: Reg -> Bytecode
emitAlloc reg = emitReg reg ++ " = alloca i32\n"

emitStore :: Reg -> Reg -> Bytecode
emitStore value ptr = "store i32 " ++ emitReg value ++ ", i32* " ++ emitReg ptr ++ "\n"

emitLoad :: Reg -> Reg -> Bytecode
emitLoad value ptr = emitReg value ++ " = " ++  "load i32,  i32* " ++ emitReg ptr ++ "\n"

emitPrint :: Reg -> Bytecode
emitPrint reg = "call void @printInt(i32 " ++ emitReg reg ++ ")\n"

emitAdd :: Reg -> Reg -> Reg -> Bytecode
emitAdd reg1 reg2 result = emitReg result ++ " = add i32 " ++ emitReg reg1 ++ ", " ++ emitReg reg2 ++ "\n"

emitSub :: Reg -> Reg -> Reg -> Bytecode
emitSub reg1 reg2 result = emitReg result ++ " = sub i32 " ++ emitReg reg1 ++ ", " ++ emitReg reg2 ++ "\n"

emitMult :: Reg -> Reg -> Reg -> Bytecode
emitMult reg1 reg2 result = emitReg result ++ " = mul i32 " ++ emitReg reg1 ++ ", " ++ emitReg reg2 ++ "\n"

emitDiv :: Reg -> Reg -> Reg -> Bytecode
emitDiv reg1 reg2 result = emitReg result ++ " = sdiv i32 " ++ emitReg reg1 ++ ", " ++ emitReg reg2 ++ "\n"

emitFileBytecode :: Bytecode -> Bytecode
emitFileBytecode bc =
    "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\""  ++
    "declare i32 @printf(i8*, ...)\n" ++
    "define void @printInt(i32 %x) {\n" ++
    "   %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n" ++
    "   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n" ++
    "   ret void\n" ++
    "}\n" ++
    "define i32 @main() {\n" ++
    bc ++
    "ret i32 0\n" ++
    "}\n"


------------------------- COMPILATION -------------------------
compileExpr :: Exp -> CompileM (Bytecode, Reg)
compileExpr (ExpLit _ n) = return ("" , Const $ fromInteger n)

compileExpr (ExpVar _ (Ident varName)) = do
    maybeReg <- getVariableReg varName
    case maybeReg of
        Just ptrReg -> do
            resultReg <- getNewReg
            return (emitLoad resultReg ptrReg, resultReg)
        Nothing -> throwError $ "Variable " ++ varName ++ " not declared. "

compileExpr (ExpAdd _ expr1 expr2) = _compileTwoOpExpr expr1 expr2 emitAdd
compileExpr (ExpSub _ expr1 expr2) = _compileTwoOpExpr expr1 expr2 emitSub
compileExpr (ExpMul _ expr1 expr2) = _compileTwoOpExpr expr1 expr2 emitMult
compileExpr (ExpDiv _ expr1 expr2) = _compileTwoOpExpr expr1 expr2 emitDiv

_compileTwoOpExpr :: Exp -> Exp -> (Reg -> Reg -> Reg -> Bytecode) -> CompileM (Bytecode, Reg)
_compileTwoOpExpr expr1 expr2 exprFunc = do
    (bc1, reg1) <- compileExpr expr1
    (bc2, reg2) <- compileExpr expr2
    resultReg <- getNewReg
    return (bc1 ++ bc2 ++ exprFunc reg1 reg2 resultReg, resultReg)

compileStmt :: Stmt -> CompileM Bytecode
compileStmt (SAss _ (Ident varName) expr) = do
    (exprBytecode, resultReg) <- compileExpr expr
    (vars, _) <- get
    case Map.lookup varName vars of
        Just ptrReg -> return $ exprBytecode ++ emitStore resultReg ptrReg
        Nothing -> throwError $ "Variable " ++ varName ++ " not declared. "

compileStmt (SExp _ expr) = do
    (bc, reg) <- compileExpr expr
    return $ bc ++ emitPrint reg

compileProgram :: Program -> CompileM Bytecode
compileProgram (Prog _ stmts) = do
    variablesBytecode <- allocVariables (Prog undefined stmts)
    stmtsBytecodes <- mapM compileStmt stmts
    return $ variablesBytecode ++ concat stmtsBytecodes


allocVariables :: Program -> CompileM Bytecode
allocVariables (Prog _ stmts) =
    let
        getVariables [] = []
        getVariables (SAss _ (Ident varName) _ : rest) = varName : getVariables rest
        getVariables (SExp _ _ : rest) = getVariables rest
        removeDuplicates = Set.toList . Set.fromList
        varList = removeDuplicates $ getVariables stmts
        addVarToState varName = do
            reg <- getNewReg
            (vars, regIdx) <- get
            put (Map.insert varName reg vars, regIdx)
            return $ emitAlloc reg ++ "\n"
    in do
        bytecodes <- mapM addVarToState varList
        return $ concat bytecodes

runCompileProgram :: Program -> Env -> ExceptT String IO Bytecode
runCompileProgram program env =
    let bc = evalStateT (runExceptT (compileProgram program)) env
        in ExceptT bc

initState :: Env
initState = (Map.empty, 0)

getBytecode :: String -> ExceptT String IO String
getBytecode code = do
    pst <- parseCode code
    bc <- runCompileProgram pst initState
    return $ emitFileBytecode bc


_compile :: String -> ExceptT String IO ()
_compile filePath = do
    let bytecodePath = replaceExtension filePath ".ll"
        bitcodePath = replaceExtension filePath ".bc"
    contents <- liftIO $ readFile filePath
    bytecode <- getBytecode contents
    liftIO $ writeToFile bytecodePath bytecode
    exitCode <- 
        liftIO $ 
        runCommandEcho $ 
        "llvm-as " ++ bytecodePath ++ " -o " ++ bitcodePath
    if exitCode /= ExitSuccess
        then throwError "LLVM assembly failed. "
        else liftIO $ putStrLn $ "Compiled binary is located in " ++ bitcodePath

compile :: String -> IO ()
compile filePath = do
    result <- runExceptT $ _compile filePath
    case result of
        Left err -> putStrLn err
        Right _ -> return ()
