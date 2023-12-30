module Main (main) where

import AOC.Strings (rcw)
import Data.List (intercalate, sort)
import Data.List.Utils (split)

main :: IO ()
main = do
  c <- getContents
  let lns = lines c
  print $ calcLoads $ tilt lns
  print $ calcLoads $ rcw $ predictCycle 1000000000 lns

-- example 1: [136,64]
-- solution : [110565,89845]

calcLoads :: [String] -> Int
calcLoads = sum . map (go 1)
  where
    go _ [] = 0
    go pos (c : cs)
      | c == 'O' = pos + go (pos + 1) cs
      | otherwise = go (pos + 1) cs

tilt :: [String] -> [String]
tilt = map (intercalate "#" . map sort . split "#") . rcw -- cheap but effective

predictCycle :: Int -> [String] -> [String]
predictCycle nth src = go
  where
    tiltCycle ss = iterate tilt ss !! 4

    -- extract the linked-list of cached of tilts which represent a cycle of calcs
    findCycle tsts [] tst = findCycle newTsts newTsts (tiltCycle tst) where newTsts = tsts++[tst]
    findCycle tsts buf@(cur:rst) tst
      | length tsts == nth = (length tsts, [tst])
      | cur == tst = (length tsts, buf)
      | otherwise = findCycle tsts rst tst

    (mth, cache) = findCycle [] [] src

    -- extrapolate what the nth tilt-cycle would be given the calc-cycle
    go = cache !! ((nth - mth) `mod` length cache)
