module Main (main) where

import Data.Char (isDigit, digitToInt)
import Data.Set (fromList, toList)

main :: IO ()
main = do
  contents <- getContents
  print (solve (lines contents))

type Pt = (Int, Int)
type Sym = (Pt, Char)
type Id = (Pt, Int)

rpt :: Pt -> [[a]] -> a
rpt (x,y) g = (g !! y) !! x

slen :: Show a => a -> Int
slen x = length (show x)

isSymbol :: Char -> Bool
isSymbol c = any (== c) "#%&*+-/=@$" -- cat input | grep -Eo '([^[:alnum:].])' | sort -u | paste -sd ''

scanSymbols :: Pt -> [String] -> [Sym]
scanSymbols _ [] = []
scanSymbols (_,y) ([]:ls) = scanSymbols (0,y+1) ls
scanSymbols (x,y) ((c:cs):ls)
  | isSymbol c = ((x,y), c):scanSymbols (x+1, y) (cs:ls)
  | otherwise  = scanSymbols (x+1, y) (cs:ls)

toId :: Pt -> Int -> Id
toId (x,y) v = ((x-(slen v), y), v)

scanIds :: Int -> Pt -> [String] -> [Id]
scanIds acc pt@(x,y) ss
  -- we've scanned everything and might have a remaining acc
  | y == length ss                   = if acc > 0 then [toId pt acc] else []
  -- we've scanned the line and acc has a value
  | x == length (ss !! y) && acc > 0 = toId pt acc:scanIds 0 (0, y+1) ss
  -- we've scanned the line and acc has no value
  | x == length (ss !! y)            = scanIds 0 (0, y+1) ss
  -- found a digit, "append" it to the current acc
  | isDigit (rpt pt ss)              = scanIds (acc*10+(digitToInt (rpt pt ss))) (x+1, y) ss
  -- not a digit, but an acc exists
  | acc > 0                          = toId pt acc:scanIds 0 (x+1, y) ss
  -- not a digit, no acc, keep scanning line
  | otherwise                        = scanIds 0 (x+1, y) ss

isAdjacent :: Id -> Sym -> Bool
isAdjacent ((ix, iy), iv) ((x, y), _) = (ix-1 <= x) && (x <= ix+(slen iv)) && (iy-1 <= y) && (y <= iy+1)

findParts :: [Id] -> [Sym] -> [Int]
findParts [] _ = []
findParts (id@(idPt, v):ids) syms
  | any (isAdjacent id) syms = v:findParts ids syms
  | otherwise                = findParts ids syms

findGears :: [Sym] -> [Id] -> [Int]
findGears [] _ = []
findGears (sym:syms) ids = let
    gs = map snd (filter (`isAdjacent` sym) ids)
  in
    product (if length gs > 1 then gs else [0]) : findGears syms ids

solve :: [String] -> [Int]
solve rs = let
    syms = (scanSymbols (0, 0) rs)
    ids  = (scanIds 0 (0, 0) rs)
  in [
    sum $ findParts ids syms,
    sum $ findGears (filter ((== '*') . snd) syms) ids
  ]

-- example 1: [4361,467835]
-- solution: [544664,84495585]
