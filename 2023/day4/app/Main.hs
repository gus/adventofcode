module Main (main) where

import Lib (solve)

main :: IO ()
main = do
  contents <- getContents
  print (solve (lines contents))
