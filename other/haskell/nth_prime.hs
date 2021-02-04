module Main where

import System.Environment
import System.Exit

-- DESCRIPTION
--   nth_prime - find the nth prime
-- SYNOPSIS
--   nth_prime n
-- BUILDING
--   $ ghc -dynamic nth_prime.hs

main :: IO ()
main = getArgs >>= parse >>= run

parse :: [String] -> IO Int
parse (n : _) = do
  let n' = read n
  if n' >= 1 then return n' else die "The input must be 1 or greater."
parse _ = die "Please specify a number."

run :: Int -> IO ()
run n = putStrLn $ "Prime #" ++ show n ++ " is " ++ result ++ "."
  where result = show $ nthPrime (n - 1)

nthPrime :: Int -> Int
nthPrime n = filter isPrime [2..] !! n

isPrime :: Int -> Bool
isPrime n
  | n > 1 = all (not . divisibleBy) [2..isqrt n]
  | otherwise = False
  where divisibleBy m = n `rem` m == 0

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
