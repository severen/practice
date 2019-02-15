module Main (main) where

import System.IO
import System.IO.Error (isDoesNotExistError)
import Control.Monad
import Control.Exception
import qualified Data.Set as Set

-- | Read a file as a string.
--
-- This function is a safe(r) wrapper around `readFile` that will return
-- `Nothing` on `isDoesNotExistError` and propagate any other exceptions.
readFile' :: String -> IO (Maybe String)
readFile' p = fmap Just (readFile p) `catch` handleError
  where
    handleError :: IOException -> IO (Maybe String)
    handleError e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e

-- | Read an integer.
--
-- This function is a specialisation of `read` to `Integer` that also handles a
-- leading '+'.
readInt :: String -> Integer
readInt ('+':xs) = read xs
readInt xs = read xs

-- | Parse input that is a string of lines of numbers into a list of integers.
parseInput :: String -> [Integer]
parseInput = map readInt . lines

part1 :: String -> Integer
part1 = sum . parseInput

-- | Find the element that is repeated first in a given list.
firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = go Set.empty
  where
    go seen (x:xs)
      | x `Set.member` seen = Just x
      | otherwise = go (x `Set.insert` seen) xs
    go _ [] = Nothing

part2 :: String -> Maybe Integer
part2 = firstRepeated . scanl (+) 0 . cycle . parseInput

run :: String -> IO ()
run input = do
  putStrLn $ "Part 1: " ++ show (part1 input)
  case part2 input of
    Just answer -> putStrLn $ "Part 2: " ++ show answer
    Nothing -> putStrLn "Part 2: Answer not found."

main :: IO ()
main = do
  input <- readFile' "day01.txt"
  case input of
    Just contents -> run contents
    Nothing -> putStrLn "Could not find input file."
