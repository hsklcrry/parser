module Lib where

-- абстрактная чепуха для парсеров

-- Stream needed
type Input = String
type Structure a = a
newtype Parser a = P { parse :: Input -> [(Structure a, Input)] }

{-
class Appendable a b | a -> b where
    append :: a -> b -> a
-}
