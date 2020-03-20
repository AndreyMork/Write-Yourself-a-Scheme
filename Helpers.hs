module Helpers
  ( getReadValue
  , bitsToInteger
  ) where


getReadValue :: (Num a) => [(a, String)] -> a
getReadValue = fst . (!! 0)

bitsToInteger :: String -> Integer
bitsToInteger = bitsToInteger' 0
  where
    bitsToInteger' acc "" = acc
    bitsToInteger' acc (x:xs) = bitsToInteger' newAcc xs
      where
        newAcc = 2 * acc + bit
        bit = if x == '0' then 0 else 1
