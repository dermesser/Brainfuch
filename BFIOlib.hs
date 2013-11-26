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

showStack :: StackCode -> String
showStack ((xs,y,zs),c) = [c] ++ "  " ++ concat (showLeft ++ showCurrent ++ showRight)
    where showLeft = (map (\x -> '[' : show x ++ "]") xs)
          showCurrent = ["{" ++ show y ++ "}"]
          showRight = (map (\z -> '[' : show z ++ "]") zs)
