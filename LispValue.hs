module LispValue
  ( LispValue(..)
  , getLispInteger
  , getLispDouble
  ) where

import Data.Complex (Complex)

data LispValue = LispAtom String
               | LispList [LispValue]
               | LispDottedList [LispValue] LispValue
               | LispInteger Integer
               | LispFloat Double
               | LispRational Rational
               | LispComplex (Complex Double)
               | LispString String
               | LispCharacter Char
               | LispBool Bool
               deriving (Show)

getLispInteger :: LispValue -> Integer
getLispInteger (LispInteger n) = n

getLispDouble :: LispValue -> Double
getLispDouble (LispFloat x) = x
getLispDouble (LispInteger x) = fromIntegral x
