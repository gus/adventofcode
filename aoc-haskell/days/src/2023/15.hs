module Main (main) where

import Data.Char (ord)
import Data.List.Utils (split)

main :: IO ()
main = do
  c <- getContents
  let rawCmds = concatMap (split ",") $ lines c
  let scoreBox (bxid, lenses) = sum $ zipWith (\(_, lens) pos -> (1+bxid)*pos*lens) lenses [1..]
  print $ sum $ map hash rawCmds
  print $ sum $ map scoreBox $ process $ parseCmds rawCmds

-- example 1: [1320,145]
-- solution : [515974,265894]

hash :: String -> Int
hash = foldl (\acc n -> ((acc + ord n) * 17) `rem` 256) 0

data Cmd = Rm Int String | Add Int (String, Int) deriving (Eq, Show)

parseCmds :: [String] -> [Cmd]
parseCmds = map (go "")
  where
    go lbl [] = error ("shouldn't get here: " ++ lbl)
    go lbl ('-' : _) = Rm (hash lbl) lbl
    go lbl ('=' : cs) = Add (hash lbl) (lbl, read cs)
    go lbl (c : cs) = go (lbl ++ [c]) cs

type Lens = (String, Int)
type Box = (Int, [Lens])

process :: [Cmd] -> [Box]
process = go []
  where
    addLens [] lens = [lens]
    addLens (a@(albl, _):lenses) lens@(blbl, _)
      | albl == blbl = lens:lenses
      | otherwise = a:addLens lenses lens

    rmLens [] _ = []
    rmLens (a@(albl, _):lenses) lbl
      | albl == lbl = lenses
      | otherwise = a:rmLens lenses lbl

    addToBox [] bxid lens = [(bxid, [lens])]
    addToBox (box@(cbxid, lenses):boxes) bxid lens
      | cbxid == bxid = (bxid, addLens lenses lens):boxes
      | otherwise = box:addToBox boxes bxid lens

    rmFromBox [] _ _ = []
    rmFromBox (box@(cbxid, lenses):boxes) bxid lbl
      | cbxid == bxid = (bxid, rmLens lenses lbl):boxes
      | otherwise = box:rmFromBox boxes bxid lbl

    go boxes [] = boxes
    go boxes (Add bxid lens:cmds) = go (addToBox boxes bxid lens) cmds
    go boxes (Rm bxid lbl:cmds) = go (rmFromBox boxes bxid lbl) cmds
