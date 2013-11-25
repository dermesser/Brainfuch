module Main where

import System.IO
import Control.Concurrent

import Control.Monad.State
import Control.Monad.Writer

import BFLib.Brainfuch (Code, emptyStack, Stack)
import BFLib.BrainfuchBreakpoint

main :: IO ()
main = do
        code <- readCode
        putStr "\nOutput of script:\n"
        stacks <- interpret code
        putStr "\n\nStack states during interpretation:\n"
        putStrLn $ showStacks stacks

-- Constants

sleeptime :: Int
sleeptime = round 1e6

deleteLine :: String
deleteLine = "\r\ESC[K"

-- Code

showStacks :: [Stack] -> String
showStacks stacks = foldl1 (\a e -> a ++ "\n" ++ e) stackstrings
    where stackstrings = map showStack stacks

showStack :: Stack -> String
showStack (xs,y,zs) = concat (showLeft ++ showCurrent ++ showRight)
    where showLeft = (map (\x -> '[' : show x ++ "]") xs)
          showCurrent = ["{" ++ show y ++ "}"]
          showRight = (map (\z -> '[' : show z ++ "]") zs)

sleep :: IO ()
sleep = threadDelay sleeptime

interpret :: Code -> IO [Stack]
interpret c = liftM (snd . fst) $ runStateT (runWriterT $ bfInt c) emptyStack

readCode :: IO Code
readCode = do
    eof <- isEOF
    if eof
     then return ""
     else do
         ln <- getLine
         lns <- readCode
         return (ln++lns)
