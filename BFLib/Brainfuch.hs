module BFLib.Brainfuch where

import Data.Char
import System.IO

import Control.Monad.State

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

decPtr :: Stack -> Stack
decPtr ([],e,ys) = ([],0,e:ys)
decPtr (xs,e,ys) = (init xs,last xs,e:ys)

incPtr :: Stack -> Stack
incPtr (xs,e,[]) = (xs++[e],0,[])
incPtr (xs,e,y:ys) = (xs++[e],y,ys)

incCell :: Stack -> Stack
incCell (xs,e,ys) = (xs,e+1,ys)

decCell :: Stack -> Stack
decCell (xs,e,ys) = (xs,e-1,ys)

bfTail :: [a] -> [a]
bfTail [] = []
bfTail l = tail l

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
mPrintContent = StateT $ \s@(_,e,_) -> (putStrLn . (:[]) . chr) e >> hFlush stdout >> return ((),s)

mReadContent :: BFState ()
mReadContent = StateT $ \(xs,_,ys) -> readLn >>= \e -> return ((),(xs,e,ys))

mIsZero :: BFState Bool
mIsZero = StateT $ \s@(_,e,_) -> return (e == 0,s)

-- Parsing helpers

-- Drops the following loop (first character must be '[')
-- [abc[34]def]ghi -> ghi
bfDropLoop :: Code -> Code
bfDropLoop = dl 0
    where dl :: Int -> Code -> Code
          dl _ [] = []
          dl i (c:cs) = case c of
                            '[' -> dl (i + 1) cs
                            ']' -> dl (i - 1) cs
                            _ -> if i == 0
                                 then (c:cs)
                                 else dl i cs

-- Returns the following loop (first character must be '['; returned is the loop code without square brackets)
-- [abc[3[4]]def]ghi -> abc[3[4]]def
bfGetLoop :: Code -> Code
bfGetLoop = init . tail . gl 0
    where gl :: Int -> Code -> Code
          gl _ [] = []
          gl i (c:cs) = case c of
                            '[' -> c : gl (i + 1) cs
                            ']' -> c : gl (i - 1) cs
                            _ -> if i == 0
                                 then []
                                 else c : gl i cs


-- Interpreter

bfInt :: Code -> BFState ()
bfInt [] = return ()
bfInt allc@(c:cs) = case c of
                    '>' -> mIncPtr >> bfInt cs
                    '<' -> mDecPtr >> bfInt cs
                    '+' -> mIncCell >> bfInt cs
                    '-' -> mDecCell >> bfInt cs
                    '.' -> mPrintContent >> bfInt cs
                    ',' -> mReadContent >> bfInt cs
                    '[' -> do
                            p <- mIsZero
                            if p
                             then bfInt . bfDropLoop $ allc
                             else let loopcode = bfGetLoop allc in do
                                                                 bfInt loopcode
                                                                 bfInt (c:cs)
                    _ -> bfInt cs

-- Entry points

bfExec :: Code -> IO ()
bfExec c = runStateT (bfInt c) emptyStack >> return ()

bfExecS :: Code -> IO Stack
bfExecS c = runStateT (bfInt c) emptyStack >>= \(_,s) -> return s
