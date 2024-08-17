module Main (main) where
import Interpreter (interpretCode, interpretRepl)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> printUsage
        ["--repl"] -> interpretRepl
        [filePath] -> readFile filePath >>= interpretCode
        []     -> getContents >>= interpretCode
        _      -> invalidUsage

printUsage :: IO ()
printUsage = do
    putStrLn "Usage: emilia-lang-exe <file>"
    putStrLn "       emilia-lang-exe --repl"
    putStrLn "       emilia-lang-exe"
    putStrLn "If no file is provided, executable will read from stdin."

invalidUsage :: IO ()
invalidUsage = do
    printUsage
    exitFailure
