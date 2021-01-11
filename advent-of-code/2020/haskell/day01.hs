module Main where

main :: IO ()
main = do
  input <- lines <$> readFile "day01.txt"
  let xs = map read input :: [Int]

  putStrLn $ "Part 1: " ++ (show $ part1 xs)
  putStrLn $ "Part 2: " ++ (show $ part2 xs)

part1 :: [Int] -> Int
part1 xs = head [ x * y | x <- xs, y <- xs, x < y, x + y == 2020]

part2 :: [Int] -> Int
part2 xs = head
  [ x * y * z
  | x <- xs, y <- xs, z <- xs
  , x < y, y < z
  , x + y + z == 2020
  ]
