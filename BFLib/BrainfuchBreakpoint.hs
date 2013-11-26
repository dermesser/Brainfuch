module BFLib.BrainfuchBreakpoint where

import Control.Monad.State
import Control.Monad.Writer
import System.IO

import BFLib.Brainfuch (Code
        , Stack
        , bfTail
        , emptyStack
        , incPtr
        , decPtr
        , incCell
        , decCell
        , bfGetLoop
        , bfDropLoop)
{-
    - Syntax

    > Increment pointer
    < Decrement pointer
    + Increment contents
    - Decrement contents
    . Put cell content
    , Read cell content
    [ ] Loop ([ - Skip following part if content == 0; ] go to last [ if content != 0)

-}

-- As function: Stack -> IO ((a,[Stack]),Stack)
type BFFState = WriterT [Stack] (StateT Stack IO)

-- monadic ops

bffTell :: Stack -> BFFState ()
bffTell sc = tell [sc]

mIncPtr :: BFFState ()
mIncPtr = WriterT . StateT $ \s -> let s' = incPtr s
                                   in return (((),[]),s')

mDecPtr :: BFFState ()
mDecPtr = WriterT . StateT $ \s -> let s' = decPtr s
                                   in return (((),[]),s')

mIncCell :: BFFState ()
mIncCell = WriterT . StateT $ \s -> let s' = incCell s
                                    in return (((),[]),s')

mDecCell :: BFFState ()
mDecCell = WriterT . StateT $ \s -> let s' = decCell s
                                    in return (((),[]),s')

mPrintContent :: BFFState ()
mPrintContent = WriterT . StateT $ \s@(_,e,_) -> (putStrLn . show) e >> hFlush stdout >> return (((),[]),s)

mReadContent :: BFFState ()
mReadContent = WriterT . StateT $ \(xs,_,ys) -> readLn >>= \e -> let s' = (xs,e,ys)
                                                                 in return (((),[]),s')

mBreakPoint :: BFFState ()
mBreakPoint = WriterT . StateT $ \s -> return (((),[s]),s)

mIsZero :: BFFState Bool
mIsZero = WriterT . StateT $ \s@(_,e,_) -> return ((e == 0,[]),s)

-- Interpreter

bfInt :: Code -> BFFState ()
bfInt [] = return ()
bfInt (c:cs) = case c of
                    '>' -> mIncPtr >> bfInt cs
                    '<' -> mDecPtr >> bfInt cs
                    '+' -> mIncCell >> bfInt cs
                    '-' -> mDecCell >> bfInt cs
                    '.' -> mPrintContent >> bfInt cs
                    ',' -> mReadContent >> bfInt cs
                    '|' -> mBreakPoint >> bfInt cs
                    '[' -> do
                            p <- mIsZero
                            if p
                             then bfInt (bfTail . dropWhile (/=']') $ cs)
                             else let loopcode = takeWhile (/=']') cs in do
                                                                             bfInt loopcode
                                                                             bfInt (c:cs)
                    _ -> bfInt cs

