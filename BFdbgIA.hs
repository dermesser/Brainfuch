module Main where

import System.IO
import Control.Concurrent

import Control.Monad.State
import Control.Monad.Writer

import BFLib.Brainfuch (Code, emptyStack, Stack)
import BFLib.BrainfuchFollow

main :: IO ()
main = do
        code <- readCode
        putStr "\nOutput of script:\n"
        stacks <- interpret code
        putStr "\n\nStack states during interpretation:\n"
        mapM_ showWaitDelete (zipWith appendRotateSym (stackStrings stacks) rotatesyms)
        putStrLn ""
    where stackStrings = map showStack
          rotatesyms = concat . repeat $ progressRotateSyms
          appendRotateSym stack sym = stack ++ "   (" ++ [sym] ++ ")"

sleeptime :: Int
sleeptime = round 1e6

sleep :: IO ()
sleep = threadDelay sleeptime

sleepShort :: IO ()
sleepShort = threadDelay (sleeptime `div` 3)

deleteLine :: String
deleteLine = "\r\ESC[K"

showWaitDelete :: String -> IO ()
showWaitDelete stack = do
                        putStr deleteLine
                        hFlush stdout
                        putStr stack
                        hFlush stdout
                        sleep

-- Code

progressRotateSyms :: String
progressRotateSyms = "-\\|/-|/"

showStack :: StackCode -> String
showStack ((xs,y,zs),c) = [c] ++ "  " ++ concat (showLeft ++ showCurrent ++ showRight)
    where showLeft = (map (\x -> '[' : show x ++ "]") xs)
          showCurrent = ["{" ++ show y ++ "}"]
          showRight = (map (\z -> '[' : show z ++ "]") zs)

interpret :: Code -> IO [StackCode]
interpret c = liftM (snd . fst) $ runStateT (runWriterT (bfInt c)) emptyStack

readCode :: IO Code
readCode = do
    eof <- isEOF
    if eof
     then return ""
     else do
         ln <- getLine
         lns <- readCode
         return (ln++lns)

