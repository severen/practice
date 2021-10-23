module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

-- | Check whether string a is an anagram of string b.
isAnagram :: String -> String -> Bool
isAnagram a b = a' /= b' && sort a' == sort b'
 where
  a' = map toLower a
  b' = map toLower b

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagram
