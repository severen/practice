module Prime (nth) where

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (not . divisibleBy) [2..isqrt n]
  where divisibleBy m = n `rem` m == 0

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ toInteger $ (filter isPrime [2..]) !! (n - 1)
