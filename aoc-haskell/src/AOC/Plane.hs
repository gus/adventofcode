module AOC.Plane where

-- A Plane is a 2D list of anything. When generated from a 2D list, each
-- element is assigned a cartesian coordinate (C) such that x grows right
-- and y grows down -- negative numbers are allowed. The element value is
-- combined with a C to form a P2 (2D point); e.g. `P2 (C 1 2) '#'`, when
-- the value at row 2, column 1 of a list of strings is the char '#'.
--
--     -x
--      |
-- -y---0---y
--      |
--      x

import qualified GHC.Stack.Types

-- | C represents a coordinate
type C = (Int, Int)

-- | P2 is a coordinate with a value
data P2 a = P2 C a | Void deriving (Read, Show, Eq)

-- | toList converts a 2D list into a 1D list of P2s.
toList :: [[a]] -> [P2 a]
toList grid = [P2 (x, y) a | (y, r) <- zip [0 ..] grid, (x, a) <- zip [0 ..] r]

-- | toPlane converts a 2D list into a P2 Plane; a 2D list of P2s.
toPlane :: [[a]] -> [[P2 a]]
toPlane grid = [[P2 (x, y) a | (x, a) <- zip [0 ..] r] | (y, r) <- zip [0 ..] grid]

-- | buildPlane builds P2 Plane from a 2D list, wherein each value is constructed
--   via the given function.
buildPlane :: (a -> b) -> [[a]] -> [[P2 b]]
buildPlane fn grid = [[P2 (x, y) (fn v) | (x, v) <- zip [0 ..] r] | (y, r) <- zip [0 ..] grid]

-- | @ returns the P2 at the given C or Void if C is outside the plane.
(@) :: [[P2 a]] -> C -> P2 a
(@) g (x, y)
  | x < 0 || x > maxx || y < 0 || y > maxy = Void
  | otherwise = g !! y !! x
  where
    (maxx, maxy) = (width g - 1, height g - 1)

-- | height returns the max y value
height :: [[P2 a]] -> Int
height = length

-- | width returns the max x value
width :: [[P2 a]] -> Int
width [] = 0
width (r:_) = length r

-- | row returns a list of values at the given row (0-indexed).
row :: (GHC.Stack.Types.HasCallStack) => [[a]] -> Int -> [a]
row = (!!)

-- | col returns a list of values at the given column (0-indexed).
col :: (GHC.Stack.Types.HasCallStack) => [[a]] -> Int -> [a]
col p x = map (!! x) p

-- | val returns the stored value of the given P2.
val :: P2 a -> a
val Void = error "(P2 Void) has no val"
val (P2 _ v) = v

-- | val returns a list of valus from the given P2s.
vals :: [P2 a] -> [a]
vals = map val

-- | Dir indicates a cardinal direction relative to the plane
data Dir = N | S | E | W deriving (Show, Eq)

-- | P2Dir represents a P2 moving/pointing in a specific direction
type P2Dir a = (P2 a, Dir)

-- | neighborC returns the C neighboring the given C in the given Dir.
neighborC :: C -> Dir -> C
neighborC (x, y) N = (x, y - 1)
neighborC (x, y) S = (x, y + 1)
neighborC (x, y) E = (x + 1, y)
neighborC (x, y) W = (x - 1, y)

-- | neighbor returns to the next P2 from the given P2 in the given Dir. If next P2
--   is outside the plane, a (P2, Void) is returned
neighbor :: [[P2 a]] -> P2Dir a -> P2Dir a
neighbor _ p2@(Void, _) = p2
neighbor g (P2 c _, d) = (g @ neighborC c d, d)

-- | pretty is a hacky attempt at printing the values of a Plane ... prettily.
pretty :: (Show a) => [[P2 a]] -> IO ()
pretty [] = putStrLn ""
pretty (r : p2s) = do
  _ <- putStr $ filter (/= '"') $ show $ vals r
  putStrLn ""
  pretty p2s
