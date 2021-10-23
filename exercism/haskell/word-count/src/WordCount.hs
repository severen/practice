module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List.Split
import Data.Map (Map)

import qualified Data.Map.Strict as Map

isSeparator :: Char -> Bool
isSeparator c = c /= '\'' && (not . isAlphaNum) c

unquote :: String -> String
unquote ('\'' : s)
  | last s == '\'' = init s
  | otherwise = s
unquote s = s

wordCount :: String -> Map String Int
wordCount = Map.fromListWith (+) . freqList . wordList
 where
  freqList = map (\s -> (map toLower s, 1))
  wordList = map unquote . wordsBy isSeparator
