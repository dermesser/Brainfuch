module BrainfuchFollow where

import Control.Monad.State
import Control.Monad.Writer
import System.IO

import Brainfuch (Code
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
mIncPtr = WriterT $ StateT $ \s -> return (((),[(incPtr s,'>')]),incPtr s)

mDecPtr :: BFFState ()
mDecPtr = WriterT $ StateT $ \s -> return (((),[(decPtr s,'<')]),decPtr s)

mIncCell :: BFFState ()
mIncCell = WriterT $ StateT $ \s -> return (((),[(incCell s,'+')]),incCell s)

mDecCell :: BFFState ()
mDecCell = WriterT $ StateT $ \s -> return (((),[(decCell s,'-')]),decCell s)

mPrintContent :: BFFState ()
mPrintContent = WriterT $ StateT $ \s@(_,e,_) -> (putStr . show) e >> return (((),[(s,'.')]),s)

mReadContent :: BFFState ()
mReadContent = WriterT $ StateT $ \s@(xs,_,ys) -> readLn >>= \e -> return (((),[(s,',')]),(xs,e,ys))

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

-- Entry points

bfExec :: Code -> IO ()
bfExec c = runStateT (runWriterT (bfInt c)) emptyStack >> return ()

bfExecS :: Code -> IO Stack
bfExecS c = runStateT (runWriterT (bfInt c)) emptyStack >>= \(_,s) -> return s
