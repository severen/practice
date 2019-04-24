module Pangram (isPangram) where

import Data.Char (toLower)

import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram cs = Set.fromList ['a'..'z'] `Set.isSubsetOf` Set.fromList (map toLower cs)
