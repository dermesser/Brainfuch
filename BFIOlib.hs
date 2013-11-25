module BFIOlib where

import System.IO
import BFLib.Brainfuch
import BFLib.BrainfuchFollow

readCode :: IO Code
readCode = do
    eof <- isEOF
    if eof
     then return ""
     else do
         ln <- getLine
         lns <- readCode
         return (ln++lns)
