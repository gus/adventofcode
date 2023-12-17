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

-- | C represents a Coordinate
data C = C !Int !Int deriving (Read, Show, Ord, Eq)

-- | P2 is a coordinate with a value
data P2 a = P2 C a deriving (Read, Show, Eq)

-- | toList converts a 2D list into a 1D list of P2s.
toList :: [[a]] -> [P2 a]
toList grid = [P2 (C x y) a | (y, r) <- zip [0 ..] grid, (x, a) <- zip [0 ..] r]

-- | toPlane converts a 2D list into a P2 Plane; a 2D list of P2s.
toPlane :: [[a]] -> [[P2 a]]
toPlane grid = [[P2 (C x y) a | (x, a) <- zip [0 ..] r] | (y, r) <- zip [0 ..] grid]

-- | row returns a list of values at the given row (0-indexed).
row :: (GHC.Stack.Types.HasCallStack) => [[a]] -> Int -> [a]
row = (!!)

-- | col returns a list of values at the given column (0-indexed).
col :: (GHC.Stack.Types.HasCallStack) => [[a]] -> Int -> [a]
col p x = map (!! x) p

-- | val returns the stored value of the given P2.
val :: P2 a -> a
val (P2 _ v) = v

-- | val returns a list of valus from the given P2s.
vals :: [P2 a] -> [a]
vals = map val

-- | pretty is a hacky attempt at printing the values of a Plane ... prettily.
pretty :: (Show a) => [[P2 a]] -> IO ()
pretty [] = putStrLn ""
pretty (r : p2s) = do
  _ <- putStr $ filter (/= '"') $ show $ vals r
  putStrLn ""
  pretty p2s
