module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

-- | Check whether string a is an anagram of string b.
isAnagram :: String -> String -> Bool
isAnagram a b = a' /= b' && sort a' == sort b'
  where
    a' = map toLower a
    b' = map toLower b

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagram
