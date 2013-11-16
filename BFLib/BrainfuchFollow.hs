module BFLib.BrainfuchFollow where

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
        , decCell)
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

type StackCode = (Stack,Char)

type BFFState = WriterT [StackCode] (StateT Stack IO)

-- monadic ops

mIncPtr :: BFFState ()
mIncPtr = WriterT $ StateT $ \s -> let s' = incPtr s
                                   in return (((),[(s','>')]),s')

mDecPtr :: BFFState ()
mDecPtr = WriterT $ StateT $ \s -> let s' = decPtr s
                                   in return (((),[(s','<')]),s')

mIncCell :: BFFState ()
mIncCell = WriterT $ StateT $ \s -> let s' = incCell s
                                    in return (((),[(s','+')]),s')

mDecCell :: BFFState ()
mDecCell = WriterT $ StateT $ \s -> let s' = decCell s
                                    in return (((),[(s','-')]),s')

mPrintContent :: BFFState ()
mPrintContent = WriterT $ StateT $ \s@(_,e,_) -> (putStr . show) e >> return (((),[(s,'.')]),s)

mReadContent :: BFFState ()
mReadContent = WriterT $ StateT $ \(xs,_,ys) -> readLn >>= \e -> let s' = (xs,e,ys)
                                                                 in return (((),[(s',',')]),s')

mIsZero :: BFFState Bool
mIsZero = WriterT $ StateT $ \s@(_,e,_) -> return ((e == 0,[]),s)

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
                    '[' -> do
                            p <- mIsZero
                            if p
                             then bfInt (bfTail . dropWhile (/=']') $ cs)
                             else let loopcode = takeWhile (/=']') cs in do
                                                                             bfInt loopcode
                                                                             bfInt (c:cs)
                    _ -> bfInt cs

