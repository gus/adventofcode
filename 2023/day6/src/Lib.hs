module Lib (solve) where

calcD :: (Int, Int) -> Int
calcD (t, d) =
  foldl (\acc c -> if (c * (t - c)) > d then acc + 1 else acc) 0 [1 .. t]

solve :: [String] -> [Int]
solve ss =
  let p1times = map read $ words $ drop 5 (head ss)
      p1dists = map read $ words $ drop 9 (last ss)
      p2time = read $ foldl1 (++) $ map show p1times
      p2dist = read $ foldl1 (++) $ map show p1dists
   in [ product $ zipWith (curry calcD) p1times p1dists, -- part 1
        calcD (p2time, p2dist) -- part 2
      ]

-- example: [288,71503]
-- solution: [219849,29432455]
