module Utilities where
import System.IO
import System.Process
import GHC.IO.Exception (ExitCode)

writeToFile :: FilePath -> String -> IO ()
writeToFile path content = do
    handle <- openFile path WriteMode  -- Open the file in write mode
    hPutStr handle content              -- Write the content to the file
    hClose handle                       -- Close the file handle

runCommandEcho :: String -> IO ExitCode
runCommandEcho command = do
    putStrLn $ "[CMD] " ++ command
    ph <- runCommand command
    waitForProcess ph

runCommandSilent :: String -> IO ExitCode
runCommandSilent command = do
    ph <- runCommand command
    waitForProcess ph

escapesToHexEscapes :: String -> String
escapesToHexEscapes = concatMap escapeToHexEscape
  where
    escapeToHexEscape :: Char -> String
    escapeToHexEscape c
      | c == '\n' = "\\0A"
      | c == '\t' = "\\09"
      | c == '\r' = "\\0D"
      | c == '\b' = "\\08"
      | c == '\f' = "\\0C"
      | c == '\\' = "\\\\"
      | c == '\"' = "\\22"
      | c == '\'' = "\\27"
      | otherwise = [c]
