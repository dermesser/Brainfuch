module Brainfuch where

import Control.Monad.Trans.State
import System.IO

type Code = String

type Stack = ([Int],Int,[Int])

type BFState = StateT Stack IO

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

emptyStack :: Stack
emptyStack = ([],0,[])

incPtr :: Stack -> Stack
incPtr ([],e,ys) = ([],0,e:ys)
incPtr (xs,e,ys) = (init xs,last xs,e:ys)

decPtr :: Stack -> Stack
decPtr (xs,e,[]) = (xs++[e],0,[])
decPtr (xs,e,y:ys) = (xs++[e],y,ys)

incCell :: Stack -> Stack
incCell (xs,e,ys) = (xs,e+1,ys)

decCell :: Stack -> Stack
decCell (xs,e,ys) = (xs,e-1,ys)

-- monadic ops

mIncPtr :: BFState ()
mIncPtr = StateT $ \s -> return ((),incPtr s)

mDecPtr :: BFState ()
mDecPtr = StateT $ \s -> return ((),decPtr s)

mIncCell :: BFState ()
mIncCell = StateT $ \s -> return ((),incCell s)

mDecCell :: BFState ()
mDecCell = StateT $ \s -> return ((),decCell s)

mPrintContent :: BFState ()
mPrintContent = StateT $ \s@(_,e,_) -> (putStr . show) e >> return ((),s)

mReadContent :: BFState ()
mReadContent = StateT $ \(xs,_,ys) -> readLn >>= \e -> return ((),(xs,e,ys))

mIsZero :: BFState Bool
mIsZero = StateT $ \s@(_,e,_) -> return (e == 0,s)

-- Interpreter

bfInt :: Code -> BFState ()
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
                            then bfInt (tail . dropWhile (/=']') $ cs)
                            else let loopcode = takeWhile (/=']') cs in do
                                                                            bfInt loopcode
                                                                            bfInt (c:cs)
                    _ -> bfInt cs

-- Entry points

bfExec :: Code -> IO ()
bfExec c = runStateT (bfInt c) emptyStack >> return ()

bfExecS :: Code -> IO Stack
bfExecS c = runStateT (bfInt c) emptyStack >>= \(_,s) -> putStrLn "" >> print s >> return s
