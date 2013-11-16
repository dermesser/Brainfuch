module Main where

import System.IO

import BFLib.Brainfuch

main = do
        code <- readCode
        putStrLn "[EOF]; Interpreting code"
        stack <- bfExecS code
        putStr "\nCurrent stack:\n"
        print stack

readCode = do
    eof <- isEOF
    if eof
     then return ""
     else do
         ln <- getLine
         lns <- readCode
         return (ln++lns)
