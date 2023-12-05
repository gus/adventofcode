module Main (main) where

import Lib

main :: IO ()
main = do
  contents <- getContents
  print (solve (lines contents))
