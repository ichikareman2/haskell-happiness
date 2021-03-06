
{- Numbers into words

Here undefined is a placeholder to show you where you need
to fill in the functions. The n to the right of the function names
is the argument which will be an integer.
Fill in the implementations of the functions above so that 
wordNumber returns the English word version of the Int value.

You will first write a function that turns integers from 0-9 into
their corresponding English words, ”one,” ”two,” and so on.
Then you will write a function that takes the integer, separates
the digits, and returns it as a list of integers. Finally you will
need to apply the first function to the list produced by the second
function and turn it into a single string with interspersed
hyphens.

We’ve laid out multiple functions for you to consider as you
tackle the problem. You may not need all of them, depending
on how you solve it — these are suggestions. Play with
them and look up their documentation to understand them
in deeper detail. You will probably find this difficult.
div :: Integral a => a -> a -> a
mod :: Integral a => a -> a -> a
map :: (a -> b) -> [a] -> [b]
concat :: [[a]] -> [a]
intersperse :: a -> [a] -> [a]
(++) :: [a] -> [a] -> [a]
(:[]) :: a -> [a]
Also consider:
Prelude> div 135 10
13
Prelude> mod 135 10
5
Prelude> div 13 10
1
Prelude> mod 13 10
3

Here is what your output should look in the REPL when it’s
working:
Prelude> wordNumber 12324546
"one-two-three-two-four-five-four-six"
Prelude>
-}
module WordNumber where
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
wordNumber = concat . map digitToWord . digits