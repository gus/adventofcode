module Main (main) where

import Debug.Trace (traceShowId)

-- scratch pad for testing things out

main :: IO ()
main = do
  print $ f

f = z 
  where
    rec x = [6,7,8,9,10,11] !! x
 
    z = [ go nn | nn <- [0..5]]

    go n =
      let
        y = rec n
      in
        case n of
          5 -> y
          _ -> n
        
