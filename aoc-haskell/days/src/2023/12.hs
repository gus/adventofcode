module Main (main) where

import Data.List (intercalate, tails)

parseInput :: [String] -> [(String, [Int])]
parseInput = map ((\s -> (head s, read ("[" ++ last s ++ "]"))) . words)

main :: IO ()
main = do
  c <- getContents
  let recs = parseInput (lines c)
  print $ sum $ map (uncurry countPerms) recs
  print $ sum $ [countPerms (intercalate "?" $ replicate 5 r) (concat $ replicate 5 gs) | (r, gs) <- recs]

-- example 1: [6,6]
-- example 2: [21,525152]
-- ghonzo:    [7922,18093821750095]
-- solution:  [7718,128741994134728]

countPerms :: [Char] -> [Int] -> Int
countPerms recs grps = head sts
  where
    (tss, tgs) = (tails recs, tails grps)
    -- states (sts) will be lazily constructed and used for memoization
    sts = [go cs gs | cs <- tss, gs <- tgs]
    -- grabs element at idx `x` from sts; if not yet computed, sts will be computed up to `x`
    -- and be available/memoized for subsequent lookups
    rec s g = sts !! ((length tss - length s - 1) * length tgs + (length tgs - length g - 1))

    go :: [Char] -> [Int] -> Int
    go [] [] = 1
    go [] _ = 0
    go cs [] = if '#' `elem` cs then 0 else 1
    go (c : cs) gg@(g : gs) =
      case c of
        '.' -> rec cs gg
        '#' -> collectGrp (g - 1) cs gs
        '?' -> rec cs gg + collectGrp (g - 1) cs gs -- treat c as '.' + treat c as '#'
        _ -> error "impossible situation"

    collectGrp :: Int -> [Char] -> [Int] -> Int
    collectGrp 0 [] [] = 1
    collectGrp 0 (c : cs) gs
      | c == '#' = 0
      | otherwise = rec cs gs
    collectGrp g [] gs = if g > 0 then 0 else rec [] gs
    collectGrp g (c : cs) gs
      | c == '.' = 0
      | otherwise = collectGrp (g - 1) cs gs
