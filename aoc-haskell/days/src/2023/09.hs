module Main (main) where

main :: IO ()
main = do
  contents <- getContents
  print (solve [map read (words s) | s <- lines contents])

diffs :: [Int] -> [Int]
diffs [p, n] = [n - p]
diffs (p : n : ns) = n - p : diffs (n : ns)
diffs _ = []

nextr :: [Int] -> Int
nextr [] = 0
nextr rs
  | all (== 0) rs = 0
  | otherwise = last rs + nextr (diffs rs)

solve :: [[Int]] -> [Int]
solve readings = map sum [map nextr readings, map (nextr . reverse) readings]

-- example 1: [114,2]
-- solution:  [1938800261,1112]
