{-# LANGUAGE RankNTypes #-}
module TestTypes where

import Data.List

data MyType = Val1 | Val2 | BadVal

testf :: (forall a. [a] -> a) -> [a] -> a
testf f list = f list

badF :: [MyType] -> MyType
badF = \_ -> BadVal

--badF2 :: forall a. [a] -> a
badF2 = \_ -> BadVal


goodF :: forall a. [a] -> a
goodF (x:_) = x

{-
action :: MyType -> Int
action Val1 = 1
action Val2 = 2
action BadVal = undefined
-}

doOnSelected :: (Integer -> Integer) -> (forall a. [a] -> a) -> Integer
doOnSelected action selector = action $ selector ([1..] :: [Integer])

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- pure f <*> x === f <$> x
--
