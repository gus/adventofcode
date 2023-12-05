module Lib (
  calibrateInputs
) where

import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, findIndex)
import Data.Maybe (fromJust, isJust)

whichDigit :: String -> Maybe Int
whichDigit s = findIndex (`isPrefixOf` s) ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

wordsToDigits :: String -> [Int]
wordsToDigits [] = []
wordsToDigits s@(x:xs)
  | isJust idx = fromJust idx:wordsToDigits xs
  | isDigit x = digitToInt x:wordsToDigits xs
  | otherwise = wordsToDigits xs
    where idx = whichDigit s

calibrateInput :: [Int] -> Int
calibrateInput [] = 0
calibrateInput xs = head xs * 10 + last xs

calibrateInputs :: [String] -> [Int]
calibrateInputs xs = [
    sum [calibrateInput (map digitToInt (filter isDigit s)) | s <- xs], -- part 1
    sum [calibrateInput (wordsToDigits s) | s <- xs] -- part 2
  ]

-- example 1: [142, 142]
-- example 2: [209, 281]
-- solution: [54450, 54265]
