-- Reverse.hs
module Reverse where

rvrs :: String -> String
rvrs x = concat [c, " ", b, " ", a, " "]
  where a = take 5 x
        b = drop 6 $  take 8 x
        c = drop 9 x

main :: IO ()
main = print $ rvrs "Curry is awesome"