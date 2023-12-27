module Main (main) where

import AOC (fnz)
import AOC.Strings (rcw)

main :: IO ()
main = do
  c <- getContents
  let ptns = parsePatterns c
  let solve n = sum $ map (\ptn -> fnz [mirrorPos n (rcw ptn), 100 * mirrorPos n ptn]) ptns
  print [solve 0, solve 1]

-- example 1: [405,400]
-- solution : [39939,32069]

parsePatterns :: String -> [[String]]
parsePatterns c = go [] (lines c)
  where
    go buf [] = [buf | not (null buf)]
    go buf ("" : ss) = buf : go [] ss
    go buf (s : ss) = go (buf++[s]) ss

mirrorPos :: Int -> [String] -> Int
mirrorPos diffs ptn = go [head ptn] (tail ptn)
  where
    offby as bs = sum $ zipWith (\a b -> if a == b then 0 else 1) as bs
    go _ [] = 0
    go buf trl@(r : rs)
      | diffs == sum (zipWith offby buf trl) = length buf
      | otherwise = go (r : buf) rs
