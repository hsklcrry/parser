{-# LANGUAGE LambdaCase, RankNTypes #-}
module Lib where

import Data.List
-- абстрактная чепуха для парсеров

-- Stream needed
type Input = String
type Structure a = a
newtype Parser a = P { parse :: Input -> [(Structure a, Input)] }

newtype ParserE a e = PE { parseE :: Input -> e -> [(Structure a, Input)]}

{-

class Appendable a b | a -> b where
    append :: a -> b -> a

-}

empty :: Parser a
empty = P (const [])

whiteSpace :: Parser String
whiteSpace = character ' '

openBracket :: Parser String
openBracket = character '('

character :: Char -> Parser String
character t = P $ \case
    (c:cs) | c == t -> [([c], cs)]
    _ -> []

word :: String -> Parser String
word w = P $ \input ->
    if w `isPrefixOf` input then [(w, drop (length w) input)]
    else []

instance Semigroup a => Semigroup (Parser a) where
    (P a) <> (P b) = P $ \input -> a input <> b input

instance Monoid a => Monoid (Parser a) where
    mempty = empty

oneOrNone :: Parser a -> Parser a
oneOrNone (P p) = undefined