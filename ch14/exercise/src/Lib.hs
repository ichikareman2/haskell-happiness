module Lib where

import Data.List (intersperse)
digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "zero"

digits :: Int -> [Int]
digits n = go n []
  where go n ns
         | n == 0 = ns
         | otherwise = go (dived n) ((moded n):ns)
        dived n = div n 10
        moded n = mod n 10

wordNumber :: Int-> String
wordNumber = concat
  . intersperse "-"
  . map digitToWord
  . digits

half x = x / 2
halfIdentity = (*2) .half
