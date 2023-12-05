module Main (
  main
) where

import Lib

main :: IO ()
main = do
  contents <- getContents
  print (calibrateInputs (lines contents))
