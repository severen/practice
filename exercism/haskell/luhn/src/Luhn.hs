module Luhn (isValid) where

import Data.Char

isValid :: String -> Bool
isValid n
  | length n' <= 1 = False
  | otherwise = luhn n' `rem` 10 == 0
  where
    n' = map digitToInt $ reverse $ filter isDigit n
    luhn (x:y:xs) = x + (if y < 5 then 2 * y else 2 * y - 9) + luhn xs
    luhn [x] = x
    luhn [] = 0
