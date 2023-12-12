module Main (main) where

type Node = (String, (String, String))

main :: IO ()
main = do
    contents <- getContents
    print (solve (lines contents))

takes :: [Int] -> [a] -> [a]
takes [] _ = []
takes _ [] = []
takes (n:an) as = as !! n : takes an as

parseNode :: String -> Node
parseNode s = (take 3 s, (takes [7..9] s, takes [12..14] s))

dfn :: Char -> ((a, a) -> a)
dfn 'L' = fst
dfn _   = snd

walk :: (String, String -> Bool) -> String -> [Node] -> Int
walk _ [] _ = 0
walk (node, atEnd) (d:path) nodes
    | atEnd node = 0
    | otherwise = case lookup node nodes of
        Nothing -> 0
        Just neighbors -> 1 + walk (dfn d neighbors, atEnd) path nodes 

solve :: [String] -> [Int]
solve ss = let
        path = cycle (head ss) -- "infinite" path
        nodes = map parseNode (drop 2 ss)
        starts = filter (\node -> 'A' == node !! 2) $ map fst nodes
    in [
        walk ("AAA", (== "ZZZ")) path nodes, -- part 1
        foldl lcm 1 $ map (\snode -> walk (snode, \node -> node !! 2 == 'Z') path nodes) starts
    ]

-- example 1: [2,2]
-- example 2: [6,6]
-- example 3: [0,6] (part 1 doesn't actually work here)
-- solution:  [20221,14616363770447]