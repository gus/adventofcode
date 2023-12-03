module Main where

import qualified Day1 (calibrateInputs)

main :: IO ()
main = do
  contents <- getContents
  print (Day1.calibrateInputs (lines contents))
