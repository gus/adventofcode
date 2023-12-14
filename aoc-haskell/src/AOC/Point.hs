module AOC.Point where

-- module AOC.Point (
--   -- boundary checks
--   (@><),

--   -- lookup
--   (@), (@?), (@?>),

--   -- movement
--   (@-), (@+), (@\), (@/)
-- ) where

import qualified GHC.Stack.Types

type Pt = (Int, Int)

-- is Pt within bounds of grid
(@><) :: [[a]] -> Pt -> Bool
(@><) [] _ = False
(@><) g@(r : _) (x, y) = y >= 0 && y < length g && x >= 0 && x < length r

-- return value at Pt
(@) :: (GHC.Stack.Types.HasCallStack) => [[a]] -> Pt -> a
(@) g (x, y) = g !! y !! x

-- find first Pt for a given value starting from (0,0)::Pt
(@?) :: (Eq a) => [[a]] -> a -> Maybe Pt
(@?) g = g @?> (0, 0)

-- find first Pt for a given value starting from given Pt
(@?>) :: (Eq a) => [[a]] -> Pt -> a -> Maybe Pt
(@?>) g pt@(x, y) c
  | y >= length g = Nothing
  | x >= length (g !! y) = (@?>) g (0, y + 1) c
  | g @ pt == c = Just pt
  | otherwise = (@?>) g (x + 1, y) c

-- adjust Pt back one column
(@-) :: Pt -> Int -> Pt
(@-) (x, y) n = (x - n, y)

-- adjust Pt forward one column
(@+) :: Pt -> Int -> Pt
(@+) (x, y) n = (x + n, y)

-- adjust Pt up one row
(@\) :: Pt -> Int -> Pt
(@\) (x, y) n = (x, y - n)

-- adjust Pt down one row
(@/) :: Pt -> Int -> Pt
(@/) (x, y) n = (x, y + n)
