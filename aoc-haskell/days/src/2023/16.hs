{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.List (nub)

import AOC.Plane

main :: IO ()
main = do
  c <- getContents
  let g = toPlane (lines c)
  let p1 = tracePath g (head $ head g, E)
  -- this is an ugly, brutish thing. would be better to cache scores for paths
  -- already traversed.
  let p2tsts = concat $ zipWith (\r d -> map (,d) r) [row g 0, col g 0, row g (height g - 1), col g (width g - 1)] [S,E,N,W]

  print $ length p1
  print $ maximum $ map (length . tracePath g) p2tsts

-- example 1: [46,51]
-- solution : [6902,7697]

tracePath :: [[P2 Char]] -> P2Dir Char -> [P2 Char]
tracePath gg pp = nub $ map fst $ go [] gg pp
  where
    fork (da, db) pth g p2d@(p2, d)
      | p2d `elem` pth = pth
      | d == da || d == db = go (p2d:pth) g (neighbor g (p2, d))
      | otherwise = go (go (p2d:pth) g (neighbor g (p2, da))) g (neighbor g (p2, db))

    bsr d = case d of N -> W; S -> E; E -> S; W -> N
    fsr d = case d of N -> E; S -> W; E -> N; W -> S

    go :: [P2Dir Char] -> [[P2 Char]] -> P2Dir Char -> [P2Dir Char]
    go pth _ (Void, _) = pth
    go pth g p2d@(p2@(P2 _ v), d)
      | v == '.' = go (p2d:pth) g (neighbor g p2d)
      | v == '\\' = go (p2d:pth) g (neighbor g (p2, bsr d))
      | v == '/' = go (p2d:pth) g (neighbor g (p2, fsr d))
      | v == '-' = fork (W, E) pth g p2d
      | v == '|' = fork (N, S) pth g p2d
    go _ _ _ = error "took a wrong turn somewhere"
