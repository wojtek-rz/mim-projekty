module Main (main) where
import System.Environment (getArgs)
import JVMCompiler (compile)

main :: IO ()
main = do 
    args <- getArgs
    case args of
        [filePath] -> compile filePath
        _ -> putStrLn "Usage: stack run <file>"