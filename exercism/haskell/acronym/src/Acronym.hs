module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

-- | Remove apostrophies and replace special characters with whitespace.
clean :: String -> String
clean = map (\c -> if isAlpha c then c else ' ') . filter (/= '\'')

-- | Uppercase the first character of a given string.
capitalize :: String -> String
capitalize (x : xs) = toUpper x : xs
capitalize "" = ""

-- | Trim an all caps string down to its first character.
trimAllCaps :: String -> String
trimAllCaps string
  | length string == length (filter isUpper string) = take 1 string
  | otherwise = string

-- | Abbreviate a given word.
abbreviate :: String -> String
abbreviate = filter isUpper . concatMap (trimAllCaps . capitalize) . words . clean
