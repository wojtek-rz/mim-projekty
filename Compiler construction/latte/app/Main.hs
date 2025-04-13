module Main (main) where
import Compiler (compile, generateIRGraphFunction, generateSSAGraphFunction, checkTypes)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import GHC.IO.Handle.Text
import System.IO (stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> printUsage
        ["--graph-ssa", funName, filePath] -> do
            result <- generateSSAGraphFunction filePath funName
            case result of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right output -> hPutStrLn stderr output
        ["--graph", funName, filePath] -> do
            result <- generateIRGraphFunction filePath funName
            case result of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right output -> hPutStrLn stderr output
        ["--typecheck", filePath] -> do
            result <- checkTypes filePath
            case result of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right output -> hPutStrLn stderr output
        [filePath] -> do
            result <- compile filePath
            case result of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right output -> hPutStrLn stderr output
        []     -> do
            code <- getContents
            result <- compile code
            case result of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right output -> hPutStrLn stderr output
        _      -> invalidUsage

printUsage :: IO ()
printUsage = do
    putStrLn "Usage: latc <file>"
    putStrLn "If no file is provided, executable will read from stdin."

invalidUsage :: IO ()
invalidUsage = do
    printUsage
    exitFailure
