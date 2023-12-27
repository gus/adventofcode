module AOC.Strings where

import Data.List (transpose)

rcw :: [[a]] -> [[a]]
rcw = map reverse . transpose

rccw :: [[a]] -> [[a]]
rccw = transpose . map reverse

r180 :: [[a]] -> [[a]]
r180 = map reverse . reverse

reflect :: [[a]] -> [[a]]
reflect = reverse
