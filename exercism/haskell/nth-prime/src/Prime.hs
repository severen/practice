module Prime (nth) where

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (\m -> n `mod` m /= 0) [2..(isqrt n)]

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ toInteger $ (filter isPrime [2..]) !! (n - 1)
