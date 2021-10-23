{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import Data.Char (isAlpha, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

responseFor :: Text -> Text
responseFor sentence
  | isShouted && isQuestion = "Calm down, I know what I'm doing!"
  | isQuestion = "Sure."
  | isShouted = "Whoa, chill out!"
  | isSilent = "Fine. Be that way!"
  | otherwise = "Whatever."
 where
  sentence' = T.strip sentence
  letters = T.filter isAlpha sentence'
  isQuestion = "?" `T.isSuffixOf` sentence'
  isShouted = T.all isUpper letters && not (T.null letters)
  isSilent = T.null sentence'
