module Main (main) where

import AOC (btw, maxmin)
import AOC.Plane (P2 (..), col, toList)
import Data.List (nub)

main :: IO ()
main = do
  c <- getContents
  let fxs = gravEfx (lines c)
  let galaxies = filter (\(P2 _ v) -> v == '#') $ toList (lines c)
  print $ map (\mag -> minDist mag fxs galaxies) [2, 10, 100, 1000000]

-- expansion:       *2      *10     *100         *1e6
-- example 1: [    374,    1030,    8410,    82000210]
-- solution:  [9639160,15662584,83426104,752936133304]

gravEfx :: [String] -> ([Int], [Int])
gravEfx [] = ([], [])
gravEfx rs = (emptyCols, emptyRows)
  where
    emptyCols = filter (\x -> nub (col rs x) == ".") [0 .. length (head rs) - 1]
    emptyRows = map snd $ filter (\(r, _) -> nub r == ".") $ zip rs [0 ..]

minDist :: Int -> ([Int], [Int]) -> [P2 Char] -> Int
minDist _ _ [] = 0
minDist _ _ [_] = 0
minDist mag fxs (P2 (x, y) _ : p2s) = sum dists + minDist mag fxs p2s
  where
    delta fx (a, b) = a + (mag - 1) * length (filter (\c -> btw b c a) fx) - b
    dists = [delta (fst fxs) (maxmin (nx, x)) + delta (snd fxs) (maxmin (ny, y)) | P2 (nx, ny) _ <- p2s]
