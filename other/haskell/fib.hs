{-# LANGUAGE BangPatterns #-}

module Main where

import System.Environment
import System.Exit

-- DESCRIPTION
--   fib - find the nth fibonacci number
-- SYNOPSIS
--   fib n
-- BUILDING
--   $ ghc -dynamic fib.hs

main :: IO ()
main = getArgs >>= parse >>= run

parse :: [String] -> IO Int
parse (n : _) = do
  let n' = read n
  if n' >= 1
    then return n'
    else die "The input must be 1 or greater."
parse _ = die "Please specify a number."

run :: Int -> IO ()
run n = putStrLn $ "Fibonacci number #" ++ show n ++ " is " ++ result ++ "."
 where
  result = show $ fib (n - 1)

fib :: Int -> Int
fib n = go n (0, 1)
 where
  go !n (!a, !b)
    | n == 0 = a
    | otherwise = go (n - 1) (b, a + b)

-- There is also this iterative implementation:
-- fib :: Int -> Int
-- fib = (fibs !!)
--   where
--     fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
