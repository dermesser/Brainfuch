module Main where

import System.IO
import BFIOlib
import BFLib.Brainfuch

main = do
        code <- readCode
        putStrLn "[EOF]; Interpreting code"
        stack <- bfExecS code
        putStr "\nCurrent stack:\n"
        print stack
