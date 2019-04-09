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
sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if x`mod`2==0 then s else x + s) 0

meanList :: [Double] -> Double
meanList l = snd ( foldr (\x (c, m) -> (c + 1, (c*m + x)/(c + 1))) (0, 0) l)

evenOnly :: [a] -> [a]
evenOnly l = snd $ unzip $ filter fst $ zip (cycle [True, False]) l --snd ( foldr (\x (c, ac) -> if c then (False, ac) else (True, x:ac)) (False, []) l)

evenOnly2 = foldr (\(c, x) ac -> if c then x:ac else ac) [] . zip (cycle [False, True])

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'

-- https://en.wikibooks.org/wiki/Haskell/Zippers

(&) :: a -> (a -> b) -> b
x & f = f x
infixl 1 &

data Node a = DeadEnd {val :: a}
    | Passage {val :: a, node :: Node a}
    | Fork {val :: a, left :: Node a, right :: Node a} deriving (Show, Eq)

get :: Node a -> a
get = val
put :: a -> Node a -> Node a
put x n = n {val = x}
update :: (a -> a) -> Node a -> Node a
update f n = n {val = n & val & f}

data Branch a = KeepStraightOn a
            | TurnLeft a (Node a)
            | TurnRight a (Node a)
type Thread a = [Branch a]

retrieve :: Thread a -> Node a -> a
retrieve []                  n             = get n
retrieve ((KeepStraightOn a):bs) (Passage v n) = retrieve bs n
retrieve ((TurnLeft _ _)     :bs) (Fork v l r)  = retrieve bs l
retrieve ((TurnRight _ _)     :bs) (Fork v l r)  = retrieve bs r

type Zipper a = (Thread a, Node a)

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Fork x l r) = Just (TurnRight x l : t, r)
turnRight _               = Nothing

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand $ e1 :*: e :+: e2 :*: e
expand (e :*: (e1 :+: e2)) = expand $ e :*: e1 :+: e :*: e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand (Val e) = Val e

