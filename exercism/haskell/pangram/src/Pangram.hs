module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List ((\\))

isPangram :: String -> Bool
isPangram word = null $ ['a' .. 'z'] \\ map toLower word
