module BnfcUtils where

import Bnfc.Abs (Ident(..))

getIdent :: Ident -> String
getIdent (Ident s) = s