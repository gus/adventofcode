module AOC.List where

-- | fold2l folds left across two lists.
fold2l :: (a -> (b, c) -> a) -> a -> [b] -> [c] -> a
fold2l _ acc [] _ = acc
fold2l _ acc _ [] = acc
fold2l fn acc (a : as) (b : bs) = fold2l fn (fn acc (a, b)) as bs
