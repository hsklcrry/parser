{-# LANGUAGE RankNTypes #-}
module TestTypes where

import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Char


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
expand e = if expand' e == e then e else expand $ expand' e

expand' :: Expr -> Expr
expand' ((e1 :+: e2) :*: e) = expand' $ e1 :*: e :+: e2 :*: e
expand' (e :*: (e1 :+: e2)) = expand' $ e :*: e1 :+: e :*: e2
expand' (e1 :+: e2) = expand' e1 :+: expand' e2
expand' (e1 :*: e2) = expand' e1 :*: expand' e2
expand' (Val e) = Val e


class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap []) = Nothing
    lookup k (ListMap ((k', v'):kvs))
        | k == k' = Just v'
        | otherwise = lookup k (ListMap kvs)
    insert k v (ListMap kvs) = ListMap $ ins' k v [] kvs
        where
            ins' k v acc [] = (k, v):acc
            ins' k v acc ((k',v'):kvs)
                | k == k' = acc ++ ((k, v) : kvs)
                | otherwise = ins' k v ((k',v'):acc) kvs
    delete k (ListMap kvs) = ListMap $ del' k [] kvs
        where
            del' k acc [] = acc
            del' k acc ((k', v') : kvs)
                | k == k' = acc ++ kvs
                | otherwise = del' k ((k', v') : acc) kvs


newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap $ const Nothing
    lookup = flip getArrowMap
    insert key v (ArrowMap m) = ArrowMap (\k -> if k == key then Just v else m k)
    delete key (ArrowMap m) = ArrowMap (\k -> if k == key then Nothing else m k)
    fromList ((k,v):xs) = insert k v (fromList xs)


data Log a = Log [String] a deriving (Show)
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg a = Log [msg] (f a)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = let
    Log messagesB b = f x
    Log messagesC c = g b
    in Log (messagesB ++ messagesC) c

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log mA a) f = let
    Log mB b = f a
    in Log (mA ++ mB) b

instance Functor Log where
    fmap f (Log ms a) = Log ms (f a)

instance Applicative Log where
    pure = Log []
    (Log mAtoB ab) <*> (Log mA a) = Log (mAtoB ++ mA) (ab a)

instance Monad Log where
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a [] = return a
execLoggersList a (l:ls) = do
    a' <- l a
    execLoggersList a' ls

add1Log a = Log ["add1"] (1+a)
mult2Log a = Log ["mul2"] (2*a)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace    
    deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно


asToken :: String -> Maybe Token
asToken [] = Nothing
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken n = if all isDigit n then Just $ Number (read n) else Nothing

tokenize :: String -> Maybe [Token]
tokenize input = if all isJust mTokens then Just $ list mTokens else Nothing
    where
        mTokens = map asToken . words $ input
        isJust (Just _) = True
        isJust _ = False
        list [] = []
        list (Just a : ms) = a : list ms
        list (Nothing : ms) = list ms



