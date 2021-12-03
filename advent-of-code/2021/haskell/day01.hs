module Main where

main :: IO ()
main = do
  input <- lines <$> readFile "day01.txt"
  let xs = map read input

  putStrLn $ "Part 1: " ++ (show $ part1 xs)
  putStrLn $ "Part 2: " ++ (show $ part2 xs)

part1 :: [Int] -> Int
part1 (x : y : tail) = (if y > x then 1 else 0) + part1 (y : tail)
part1 _ = 0

part2 :: [Int] -> Int
part2 = part1 . sum3
 where
  sum3 (x : y : z : tail) = (x + y + z) : sum3 (y : z : tail)
  sum3 _ = []
