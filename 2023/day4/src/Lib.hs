module Lib (solve) where

import Data.List.Utils (split)

parseCards :: [String] -> [Int]
parseCards [] = []
parseCards (s : ss) =
  let wn = map words $ split " | " $ last (split ": " s)
   in length (filter (`elem` head wn) (last wn)) : parseCards ss

countCopies :: Int -> Int -> [Int] -> Int
countCopies 0 acc _ = acc
countCopies _ acc [] = acc
-- could try using Data.Map here next time
countCopies i acc (n : ns) = countCopies (i - 1) (acc + 1 + countCopies n 0 ns) ns

solve :: [String] -> [Int]
solve ss =
  let cards = parseCards ss
   in [ sum
          [if n == 0 then 0 else 2 ^ (n - 1) | n <- cards], -- part 1
        countCopies (length cards) 0 cards -- part 2
      ]

-- example: [13, 30]
-- part 1: [20117, 13768818]
