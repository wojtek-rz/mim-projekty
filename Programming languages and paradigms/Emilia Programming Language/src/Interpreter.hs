module Interpreter (
    interpretCode, interpretRepl
) where
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Monad.Except 

import Bnfc.Par    ( pProgram, myLexer )
import Bnfc.Layout ( resolveLayout )
import Bnfc.Lex    ( mkPosToken )
import TypeChecker.TypeChecker (checkTypes, checkTypesRepl)
import Evaluator.Evaluator (runEval, runEvalRepl)
import Bnfc.Abs (Program)
import TypeChecker.Monad (TypeState)
import TypeChecker.Utils (entryTypeState)
import Evaluator.LangData (EvalState)
import Evaluator.Builtins (entryState)

parseCode :: String -> ExceptT String IO Program
parseCode code = 
    let tokens = myLexer code
        tokensWithBraces = resolveLayout True tokens
        parseTree = pProgram tokensWithBraces
    in case parseTree of
        Left err -> throwError $ concat ["Parse failed: \n", showPosToken $ last tokensWithBraces, "\n", err]
        Right program -> return program
    where
        showPosToken token = 
                    let ((l,c),t) = mkPosToken token in
                    concat [ show l, ":", show c, "\t", show t ]

runAll :: String -> ExceptT String IO ()
runAll code = do
    parsed <- parseCode code
    checkTypes parsed
    runEval parsed

interpretCode :: String -> IO ()
interpretCode code = do
    result <- runExceptT $ runAll code
    case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right _ -> return ()


getMoreLinesRepl :: String -> IO String
getMoreLinesRepl code = do
    line <- getLine
    if line == "" then return code
    else getMoreLinesRepl $ code ++ line ++ "\n"

getLinesRepl :: IO String
getLinesRepl = do
    putStr "-------------- enter your code below --------------\n"
    line <- getLine
    if line == "" then return ""
    else getMoreLinesRepl (line ++ "\n")

getProgramRepl :: IO Program
getProgramRepl = do
    code <- getLinesRepl
    result <- runExceptT (parseCode code)
    case result of
        Left err -> putStrLn err >> getProgramRepl
        Right program -> return program

runWithStateRepl :: Program -> TypeState -> EvalState -> IO ()
runWithStateRepl program typesState evalState = do
    typeCheckResult <- checkTypesRepl typesState program
    case typeCheckResult of
        Left err -> repeatRepl err typesState evalState
        Right newCheckState -> do
            evalResult <- runEvalRepl evalState program
            case evalResult of
                Left err -> repeatRepl err typesState evalState
                Right (result, newEvalState) -> repeatRepl ("'" ++ result ++ "'") newCheckState newEvalState
    where 
        repeatRepl message newCheckState newEvalState = do
            putStrLn message
            newProgram <- getProgramRepl
            runWithStateRepl newProgram newCheckState newEvalState

interpretRepl :: IO ()
interpretRepl = do
    putStrLn "Welcome to Emilia REPL! Press Enter twice to run the code."
    program <- getProgramRepl
    runWithStateRepl program entryTypeState entryState
