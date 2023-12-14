module Main (main) where

import Data.Maybe (fromJust)
import AOC.Point

main :: IO ()
main = do
  c <- getContents
  let grid = lines c
  let spath = findPath grid (fromJust $ grid @? 'S')
  print $ map ceiling [(/ 2) $ fromIntegral $ length spath, insidePts spath]

-- examples: 1/[4,1]  2/[8,1]  3/[23,4]  4/[70,8]  5/[80,10]
-- solution: [6927,467]
-- ghonzo (sanity check): [6773,493]

tuplets :: [a] -> [(a, a)]
tuplets [p0, p1] = [(p0, p1)]
tuplets (p0 : p1 : ps) = (p0, p1) : tuplets (p1 : ps)
tuplets _ = []

ptArea :: (Pt, Pt) -> Float
ptArea ((ax, ay), (bx, by)) = fromIntegral (bx - ax) * (fromIntegral (ay + by) / 2)

polyArea :: [Pt] -> Float
polyArea pts = abs $ sum $ map ptArea $ tuplets pts

-- | Count of points inside a polygon. Calculated as: 1 + area + Pt-count/2
insidePts :: [Pt] -> Float
insidePts path = 1 + polyArea path - (fromIntegral (length path) / 2)

ptElem :: (Eq a) => [[a]] -> Pt -> [a] -> Bool
ptElem g pt cs = g @>< pt && (g @ pt) `elem` cs

-- assumes S is definitely at an elbow
findPath :: [String] -> Pt -> [Pt]
findPath grd spt = spt : go grd spt (fst $ head $ filter (uncurry (ptElem grd)) ds)
  where
    ds = [(spt @- 1, "FL-"), (spt @+ 1, "J7-"), (spt @\ 1, "F7|"), (spt @/ 1, "LJ|")]
    go g (px, py) pt@(x, y)
      | not (g @>< pt) = error "outside of grid"
      | c == 'S' = []
      | c == '-' = pt : go g pt (x + x - px, y)
      | c == '|' = pt : go g pt (x, y + y - py)
      | c `elem` "LF" && py /= y = pt : go g pt (pt @+ 1)
      | c `elem` "J7" && py /= y = pt : go g pt (pt @- 1)
      | c `elem` "LJ" = pt : go g pt (pt @\ 1)
      | c `elem` "F7" = pt : go g pt (pt @/ 1)
      | otherwise = error "path within grid is invalid"
      where
        c = g @ pt
