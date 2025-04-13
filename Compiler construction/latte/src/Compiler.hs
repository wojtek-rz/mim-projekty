module Compiler (compile, generateIRGraphFunction, generateSSAGraphFunction, checkTypes) where

import Bnfc.Abs
import Bnfc.Par
import CodeGenerator.DotGraph (generateDotFunction)
import qualified CodeGenerator.Generator  as Generator
import LLVMBackend.SSA (programToSSA)
import Control.Monad.Except
import Control.Monad.IO.Class
import Errors.Definitions
import Parser.Errors
import ReturnChecker (runReturnCheck)
import TypeCheck.TypeChecker (runTypeCheck)
import qualified LLVMBackend.Backend
import System.FilePath (replaceExtension, takeBaseName, replaceBaseName)
import Utilities (writeToFile, runCommandSilent)
import GHC.IO.Exception (ExitCode(..))

parseCode :: String -> ExceptT String IO Program
parseCode code =
  let tokens = myLexer code
      parseTree = pProgram tokens
   in case parseTree of
        Left err -> throwError $ showError $ ParserError err
        Right program -> return program

_compile :: String -> ExceptT String IO String
_compile code = do
  parsedCode <- parseCode code
  runTypeCheck parsedCode
  runReturnCheck parsedCode
  let ir = Generator.compile parsedCode
  let compiled = LLVMBackend.Backend.generate ir
  return compiled

compile :: String -> IO (Either String String)
compile path = do
  code <- readFile path
  result <- runExceptT $ _compile code
  case result of
    Left err -> return $ Left $ "ERROR\n" ++ err
    Right code -> do
      let bytecodePath = replaceExtension path ".ll"
          bitcodePath = replaceExtension path ".bc"
      writeToFile bytecodePath code
      exitCode <-
        liftIO $
        runCommandSilent $
        "llvm-as " ++ bytecodePath ++ " -o " ++ bitcodePath
      case exitCode of
        ExitSuccess -> return $ Right "OK\n"
        ExitFailure _ -> return $ Left "ERROR\n"

generateIRGraphFunction :: String -> String -> IO (Either String String)
generateIRGraphFunction filePath functionName = 
  generateGraphFunction filePath functionName False
generateSSAGraphFunction :: String -> String -> IO (Either String String)
generateSSAGraphFunction filePath functionName = 
  generateGraphFunction filePath functionName True

generateGraphFunction :: String -> String -> Bool -> IO (Either String String)
generateGraphFunction filePath functionName useSSA = do
  code <- readFile filePath
  result <- runExceptT $ do
    parsedCode <- parseCode code
    runTypeCheck parsedCode
    runReturnCheck parsedCode
    let ir = Generator.compile parsedCode
        ir' = if useSSA then programToSSA ir else ir
        dotgraph = generateDotFunction ir' functionName
    return dotgraph

  case result of
    Left err -> return $ Left $ "ERROR\n" ++ err
    Right dotGraph -> do
      let fileBase = takeBaseName filePath
          outputBase = fileBase ++ "_" ++ functionName ++ "_" ++ (if useSSA then "ssa" else "ir")
          outputFile = replaceBaseName filePath outputBase
          dotFilePath = replaceExtension outputFile ".dot"
          dotPngPath = replaceExtension outputFile ".png"
      writeToFile dotFilePath dotGraph
      exitCode <- liftIO $ runCommandSilent $ "dot -Tpng " ++ dotFilePath ++ " -o " ++ dotPngPath
      case exitCode of
        ExitSuccess -> return $ Right "OK\n"
        ExitFailure _ -> return $ Left "ERROR\n"

checkTypes :: String -> IO (Either String String)
checkTypes path = do
  code <- readFile path
  result <- runExceptT $ do
    parsedCode <- parseCode code
    runTypeCheck parsedCode
    runReturnCheck parsedCode
    return "OK\n"
  case result of
    Left err -> return $ Left $ "ERROR\n" ++ err
    Right _ -> return $ Right "OK\n"
