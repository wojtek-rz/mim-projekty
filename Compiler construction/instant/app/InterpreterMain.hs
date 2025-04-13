module Main (main) where
import System.Environment (getArgs)

import Interpreter (interpret)

main :: IO ()
main = do 
    args <- getArgs
    case args of
        [filePath] -> readFile filePath >>= interpret
        _ -> putStrLn "Usage: stack run <file>"
