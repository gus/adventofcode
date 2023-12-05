module Lib (solve) where

import Data.String.Utils (split)

type RGB = (Int, Int, Int)
type Reveals = [RGB]
type Game = (Int, Reveals)

rgbProd :: RGB -> Int
rgbProd (r, g, b) = r*g*b

rgbInBounds :: RGB -> RGB -> Bool
rgbInBounds (cr, cg, cb) (r, g, b) = r <= cr && g <= cg && b <= cb

rgbMax :: RGB -> Reveals -> RGB
rgbMax acc [] = acc
rgbMax (ar, ag, ab) ((r, g, b):rs) = rgbMax ((max ar r), (max ag g), (max ab b)) rs

-- parsing the games

parseRGB :: [[String]] -> RGB -> RGB
parseRGB [] rgb = rgb
parseRGB ([n, "red"]:xs) (_, g, b) = parseRGB xs (read n::Int, g, b)
parseRGB ([n, "green"]:xs) (r, _, b) = parseRGB xs (r, read n::Int, b)
parseRGB ([n, "blue"]:xs) (r, g, _) = parseRGB xs (r, g, read n::Int)
parseRGB (x:_) rgb = error ("unrecognized rgb token: " ++ unwords x)

parseReveals :: [[[String]]] -> Reveals
parseReveals [] = []
parseReveals (r:rs) = parseRGB r (0, 0, 0):parseReveals rs

parseGames :: Int -> [String] -> [Game]
parseGames _ [] = []
parseGames idx (r:rs) = (idx, parseReveals ( -- tokenize Reveals
  map (map words) $ map (split ",") $ split ";" $ last $ split ":" r)) : parseGames (idx+1) rs

-- validating games (part 1)

isValidGame :: RGB -> Game -> Bool
isValidGame chk (idx, rgbs) = all (rgbInBounds chk) rgbs

-- main

solve :: [String] -> [Int]
solve rs = let games = parseGames 1 rs in [
    sum $ map fst $ filter (isValidGame (12, 13, 14)) games, -- part 1
    sum $ map rgbProd $ map (rgbMax (0, 0, 0)) $ map snd games -- part 2
  ]

-- example 1: [8, 2286]
-- solution: [2204, 71036]
