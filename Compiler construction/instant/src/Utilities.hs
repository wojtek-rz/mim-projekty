module Utilities where
import Control.Monad.Except
import Bnfc.Abs (Program, BNFC'Position)
import Bnfc.Par
import Bnfc.Lex hiding (tokens)
import System.IO
import System.Process
import GHC.IO.Exception (ExitCode)

printPos :: BNFC'Position -> String
printPos (Just (l, c)) = "[line " ++ show l ++ ", column " ++ show c ++ "]"
printPos Nothing = ""

parseCode :: String -> ExceptT String IO Program
parseCode code =
    let tokens = myLexer code
        parseTree = pProgram tokens
    in case parseTree of
        Left err -> throwError $ concat ["Parse failed: \n", showPosToken $ last tokens, "\n", err]
        Right program -> return program
    where
        showPosToken token =
                    let ((l,c),t) = mkPosToken token in
                    concat [ show l, ":", show c, "\t", show t ]

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

maxFromZero :: [Int] -> Int
maxFromZero [] = 0
maxFromZero xs = maximum xs
