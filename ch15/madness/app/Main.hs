module Main where

import Lib

main :: IO ()
main = do
  e <- getLine
  adv <- getLine
  noun <- getLine
  adj <- getLine
  do putStrLn $ madlibbinBetter' e adv noun adj
