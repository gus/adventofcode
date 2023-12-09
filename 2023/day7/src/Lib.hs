module Lib (solve) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.List.Extra (groupSortOn)
import qualified Data.Ord
import Data.Sort (sortOn)

nseq :: (Int, Int) -> Int
nseq (_, 0) = 0
nseq (n, t) = read $ foldl1 (++) $ replicate t (show n)

-- bad function name, but ... increase the snd value of the elements
-- in the first list by the snd value of the elements in the second
-- list, aligned by index; until either list runs out. whatever remains
-- of either list is appended to the tail.
combine :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
combine [] ys = ys
combine xs [] = xs
combine ((x,xn):xs) ((_,yn):ys) = (x, xn+yn):combine xs ys

-- see notes below
scoreHand :: String -> (String, Int) -> (Int, Int)
scoreHand legend (hand, bid) = let
    crdSeq = map (\c -> 1+fromMaybe (-1) (elemIndex c legend)) hand
    crdHsh = foldl1 (++) (map (show . (+) 10) crdSeq)
    grpCrds = sortOn (Data.Ord.Down . nseq) $ map (\g -> (head g+10, length g)) $ groupSortOn id crdSeq
    crdCnts = combine (filter (\c -> fst c > 10) grpCrds) (filter (\c -> fst c == 10) grpCrds)
    crdTyp = foldl1 (++) (map (\g -> show $ nseq (snd g, snd g)) crdCnts)
  in (read $ crdTyp ++ crdHsh, bid)

scoreGame :: [(Int, Int)] -> Int
scoreGame hs = sum $ zipWith (curry (\ ((_, b), r) -> b * r)) hs [1 .. (length hs)]

solve :: [String] -> [Int]
solve ss = let
    hs = map (\s -> (take 5 s, read (drop 6 s)::Int)) ss
  in map (\legend -> scoreGame $ sortOn fst $ map (scoreHand legend) hs) ["23456789TJQKA", "23456789TQKA"]

-- example: [6440,5905]
-- solution: [251029473,251003917]

-- a hands score is comprised of its 'type' and its 'hash', which together create
-- an idempotent and sortable hand-id. this acts as the hand's score which can be
-- compared and ranked against other hands.
--
-- the type is an ordered listing of how many times each card shows up in the hand.
-- for example: the hand '33KT2' as a type '22111' which indicates there is one pair.
-- whereas type('JJTTK') == '22221' which indicates two pairs.
--
-- the hash is the sequence of each card's value represented as a single number.
-- a card's value is determined by its index in the provided legend. each value is
-- increased by 10 to ensure all values have a fixed "width" of 2 digits per value.
-- thus, given the card legend '..23456789TJQKA' (a literal valuation) the hand
-- '23456' has hash '1213141516' (i.e. `map (+ 10) [2,3,4,5,6]).
--
-- concatenate the type and hash together and you get a single id which also acts
-- as the orderable score. examples using the legend '__23456789TJQKA':
--
--   hand  -> type  hash
--   33KT2 -> 22111 1313232012
--   JJTTK -> 22221 2120212023
--   555JT -> 33311 1515152120
--   66666 -> 55555 1616161616
--
-- the space between type and hash is merely for readability. for example, the
-- score for hand '33KT2' above is 221111313232012.
--
-- any card not in the legend is treated as a joker and will show up with value 10.
-- jokers are auto counted towards the highest rank card/n-of-a-kind in the hand to
-- affect the type, but maintain their position in the hash as the lowest card value.
