{-# LANGUAGE LambdaCase #-}
module Main where

import Data.String.Utils
import Lib
import Data.String.Strip

main :: IO ()
main = print "hello"

newtype Parser = P (String -> [(String, String)])

parse :: Parser -> String -> [(String, String)]
parse (P p) = p

empty :: Parser
empty = P (const [])

whiteSpace :: Parser
whiteSpace = character ' '

openBracket :: Parser
openBracket = character '('

character :: Char -> Parser
character c = P $ \case
    (c:cs) | c == c -> [([c], cs)]
    _ -> []

word :: String -> Parser
word w = P $ \input ->
    if startswith w input then [(w, drop (length w) input)]
    else []

(<+>) :: Parser -> Parser -> Parser
(P a) <+> (P b) = P $ \input -> a input ++ b input


