module Main where

main :: IO ()
main = do
  input <- readFile "day01.txt"
  let xs = read <$> lines input

  putStrLn $ "Part 1: " ++ (show $ part1 xs)
  putStrLn $ "Part 2: " ++ (show $ part2 xs)

part1 :: [Int] -> Int
part1 (x : y : xs) = (if y > x then 1 else 0) + part1 (y : xs)
part1 _ = 0

part2 :: [Int] -> Int
part2 = part1 . sum3
 where
  sum3 (x : y : z : xs) = (x + y + z) : sum3 (y : z : xs)
  sum3 _ = []
