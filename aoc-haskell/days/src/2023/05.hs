module Main (main) where

import Data.Char (isDigit)

main :: IO ()
main = do
  contents <- getContents
  print (solve (lines contents))

type CatMap = (Int, Int, Int)

parseCatMap :: String -> CatMap
parseCatMap s =
  let mw = map read $ words s
   in (head mw, mw !! 1, last mw)

parseCatMaps :: [CatMap] -> [String] -> [[CatMap]]
parseCatMaps a [] = [a]
parseCatMaps a ("" : ss) = a : parseCatMaps [] ss
parseCatMaps a (s@(c : _) : ss)
  | isDigit c = parseCatMaps (parseCatMap s : a) ss
  | otherwise = parseCatMaps a ss

mapCat :: Int -> [CatMap] -> Int
mapCat src [] = src
mapCat src ((dr, sr, bnd) : cms)
  | sr <= src && sr + bnd > src = dr + src - sr
  | otherwise = mapCat src cms

asRanges :: [Int] -> [(Int, Int)]
asRanges [] = []
asRanges [_] = error "bad seed range"
-- asRanges (s : sz : ps) = [s .. s + sz - 1] ++ asRanges ps
asRanges (s : sz : ps) = (s, sz - 1) : asRanges ps

rng :: (Int, Int) -> [Int]
rng (s, sz) = [s .. sz + s]

mapLocs :: [[CatMap]] -> [Int] -> Int
mapLocs almanacs seeds = minimum $ map (\s -> foldl mapCat s almanacs) seeds

solve :: [String] -> [Int]
solve ss =
  let seeds = map read $ tail $ words (head ss)
      almanacs = parseCatMaps [] (drop 2 ss) -- skip first two lines
   in [ mapLocs almanacs seeds,
        minimum $ map (mapLocs almanacs . rng) (asRanges seeds)
      ]

-- example: [35,46]
-- part 1: [650599855,1240035]
