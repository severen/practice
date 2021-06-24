module Pangram (isPangram) where

import Data.List ((\\))
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram word = null $ ['a'..'z'] \\ map toLower word
