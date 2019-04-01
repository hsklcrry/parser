{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List
import Lib
import Data.String.Strip

main :: IO ()
main = print "hello"

type Input = String
type Structure a = a
newtype Parser a = P (Input -> [(Structure a, Input)])

parse :: Parser a -> Input -> [(Structure a, Input)]
parse (P p) = p

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

(<+>) :: Parser a -> Parser a -> Parser a
(P a) <+> (P b) = P $ \input -> a input ++ b input

oneOrNone :: Parser a -> Parser a
oneOrNone (P p) = undefined
