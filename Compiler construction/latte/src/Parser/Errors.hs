{-# LANGUAGE InstanceSigs #-}
module Parser.Errors where
import Errors.Definitions

newtype ParserError = ParserError String

instance Error ParserError where
    title :: ParserError -> String
    title (ParserError _) = "Parser error"

    description :: ParserError -> String
    description (ParserError msg) = msg

    codeFragment :: ParserError -> CodeFragment
    codeFragment (ParserError _) = NoFragment