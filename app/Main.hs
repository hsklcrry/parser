{-# LANGUAGE LambdaCase, RankNTypes #-}
module Main where

import Lib
import Data.String.Strip
import Control.Monad.Trans
import Control.Monad.State

main :: IO ()
main = runStateT (code >> code) 0 >> print "asdf"

code :: StateT Integer IO ()
code = do
    x <- get
    liftIO $ print x
    put $ x + 10

main1 :: IO ()
main1 = evalST (testSIO >> testSIO) 0

testSIO :: StateIO Int ()
testSIO = do
    x <- getS
    liftS $ print x
    putS (x + 10)
    return ()

newtype StateIO s a = S { runST :: s -> IO (s, a)}

evalST :: StateIO s a -> s -> IO a
evalST (S v) state = do
    (_, a) <- v state
    return a

liftS :: IO a -> StateIO s a
liftS action = S $ \s -> do
    a <- action
    return (s, a)

getS :: StateIO s s
getS = S $ \s -> return (s, s)

putS :: s -> StateIO s ()
putS x = S $ \s -> return (x, ())

bindS :: StateIO s a -> (a -> StateIO s b) -> StateIO s b
bindS (S u) f = S $ \s -> do
    (s', a') <- u s
    runST (f a') s'

instance Functor (StateIO s) where
    fmap f (S u) = S $ \s -> fmap (fmap f) (u s)

instance Applicative (StateIO s) where
    pure a = S $ \s -> return (s,a)
    (S f) <*> (S a) = S $ \s -> do
        (s1, f') <- f s
        (s2, a') <- a s1
        return (s2, f' a')

instance Monad (StateIO s) where
    (>>=) = bindS

