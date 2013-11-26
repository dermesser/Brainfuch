module Main where

import BFIOlib
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
        putStrLn $ showStacks stacks

-- Code

showStacks :: [StackCode] -> String
showStacks stacks = foldl1 (\a e -> a ++ "\n" ++ e) stackstrings
    where stackstrings = map showStack stacks


interpret :: Code -> IO [StackCode]
interpret c = liftM (snd . fst) $ runStateT (runWriterT (bffTell (emptyStack,' ') >> bfInt c)) emptyStack
