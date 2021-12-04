{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (toUpper)

type Aim = Int
type Position = (Int, Int)
data Command = Forward Int | Up Int | Down Int deriving (Read)

main :: IO ()
main = do
  input <- lines <$> readFile "day02.txt"
  let cmds = map parseLine input

  putStrLn $ "Part 1: " ++ (show $ part1 cmds)
  putStrLn $ "Part 2: " ++ (show $ part2 cmds)

parseLine :: String -> Command
parseLine = read . capitalize
 where
  capitalize (c : cs) = toUpper c : cs
  capitalize _ = ""

part1 :: [Command] -> Int
part1 cmds = let (x, y) = foldl executeCommand (0, 0) cmds in x * y
 where
  executeCommand :: Position -> Command -> Position
  executeCommand (x, y) = \case
    (Forward k) -> (x + k, y)
    (Up k) -> (x, y - k)
    (Down k) -> (x, y + k)

part2 :: [Command] -> Int
part2 cmds = let (_, (x, y)) = foldl executeCommand (0, (0, 0)) cmds in x * y
 where
  executeCommand :: (Aim, Position) -> Command -> (Aim, Position)
  executeCommand (aim, (x, y)) = \case
    (Forward k) -> (aim, (x + k, y + k * aim))
    (Up k) -> (aim - k, (x, y))
    (Down k) -> (aim + k, (x, y))
