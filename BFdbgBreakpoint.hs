module Main where

import System.IO
import Control.Concurrent

import Control.Monad.State
import Control.Monad.Writer

import BFIOlib
import BFLib.Brainfuch (Code, emptyStack, Stack)
import BFLib.BrainfuchBreakpoint

main :: IO ()
main = do
        code <- readCode
        putStr "\nOutput of script:\n"
        stacks <- interpret code
        putStr "\n\nStack states during interpretation:\n"
        putStrLn $ showStacks stacks

-- Code

showStacks :: [Stack] -> String
showStacks stacks = foldl1 (\a e -> a ++ "\n" ++ e) stackstrings
    where stackstrings = map Main.showStack stacks

showStack :: Stack -> String
showStack (xs,y,zs) = concat (showLeft ++ showCurrent ++ showRight)
    where showLeft = (map (\x -> '[' : show x ++ "]") xs)
          showCurrent = ["{" ++ show y ++ "}"]
          showRight = (map (\z -> '[' : show z ++ "]") zs)

interpret :: Code -> IO [Stack]
interpret c = liftM (snd . fst) $ runStateT (runWriterT $ bfInt c) emptyStack
