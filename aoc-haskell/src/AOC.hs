module AOC where

tuplets :: [a] -> [(a, a)]
tuplets [a0, a1] = [(a0, a1)]
tuplets (a0 : a1 : ps) = (a0, a1) : tuplets (a1 : ps)
tuplets _ = []

triplets :: [a] -> [(a, a, a)]
triplets [a0, a1, a2] = [(a0, a1, a2)]
triplets (a0 : a1 : a2 : ps) = (a0, a1, a2) : triplets (a1 : a2: ps)
triplets _ = []

minmax :: Ord a => (a,a) -> (a,a)
minmax t = (uncurry min t, uncurry max t)

maxmin :: Ord a => (a,a) -> (a,a)
maxmin t = (uncurry max t, uncurry min t)

btw :: Ord a => a -> a -> a -> Bool
btw a n b = n > a && n < b

btwi :: Ord a => a -> a -> a -> Bool
btwi a n b = n >= a && n <= b

fnz :: [Int] -> Int
fnz [] = 0
fnz (0:ns) = fnz ns
fnz (n:_) = n
