module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> [Int]
factors n = filter isFactor [1..n `div` 2]
  where isFactor x = n `rem` x == 0

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | aliquot < n = Just Deficient
  | aliquot == n = Just Perfect
  | aliquot > n = Just Abundant
  | otherwise = Nothing
  where aliquot = sum $ factors n
