-- ch10.hs
module Ch10 where

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
-}

{-
-- 1. foldr (*) 1 [1..5]
(*) 1 (foldr (*) 1 [2,3,4,5])
(*) 1 ((*) 2 (foldr (*) [3,4,5]))
(*) 1 ((*) 2 ((*) 3 (foldr 1 [4,5])))
(*) 1 ((*) 2 ((*) 3 ((*) 4 (foldr 1 [5]))))
(*) 1 ((*) 2 ((*) 3 ((*) 4 ((*) 5 (foldr 1 [])))))
(*) 1 ((*) 2 ((*) 3 ((*) 4 ((*) 5 1))))
(*) 1 ((*) 2 ((*) 3 ((*) 4 5)))
(*) 1 ((*) 2 ((*) 3 20))
(*) 1 ((*) 2 60)
(*) 1 120
120
-}

{-
-- 2. foldl (flip (*)) 1 [1..3]
foldl (flip (*)) 1 [1,2,3]
foldl (flip (*)) ((flip (*)) 1 1) [2,3] 
foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3] 
foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) [] 
((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) 
((flip (*)) ((flip (*)) 1 2) 3) 
((flip (*)) 2 3) 
6 
-}

{-
-- 3. One difference between foldr and foldl is: c
a) foldr, but not foldl, traverses the spine of a list from right to left
b) foldr, but not foldl, always forces the rest of the fold
c) foldr, but not foldl, associates to the right
d) foldr, but not foldl, is recursive
-}

{-
-- 4. Folds are catamorphisms, which means they are generally used to: a
a) reduce structure
b) expand structure
c) render you catatonic
d) generate infinite data structures
-}

{-
5. The following are simple folds very similar to what you’ve
already seen, but each has at least one error. Please fix
them and test in your REPL:
a) foldr (++) ["woot", "WOOT", "woot"]
b) foldr max [] "fear is the little death"
c) foldr and True [False, True]
d) This one is more subtle than the previous. Can it ever
return a different answer?
foldr (||) True [False, True]
e) foldl ((++) . show) "" [1..5]
f) foldr const 'a' [1..5]
g) foldr const 0 "tacos"
h) foldl (flip const) 0 "burritos"
i) foldl (flip const) 'z' [1..5]
-}
a = foldr (++) "" ["woot", "WOOT", "woot"]
b = foldr max '0' "fear is the little death"
c = and [False, True]
d = foldr (||) False [False ,True]
e = foldr (flip const) 'a' [1..5]
f = foldl const 0 "tacos"
g = foldl (const) 0 "burritos"
h = foldl (const) 'z' [1..5]
{-
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls =
  q : (case ls of
    [] -> []
    x : xs -> scanl f (f q x) xs)
-}
-- Scans Exercises
{-
1. Modify your fibs function to only return the first 20 Fibonacci numbers.
2. Modify fibs to return the Fibonacci numbers that are less than 100.
3. Try to write the factorial function from Recursion as a scan.
  You’ll want scanl again, and your start value will be
  1. Warning: this will also generate an infinite list, so you
  may want to pass it through a take function or similar.
-}

-- fibs = 1 : scanl (+) 1 fibs
fibs = 1 : scanl (+) 1 fibs
{-
fibs = 1 : scanl (+) 1 fibs
1 : 1 : scanl (+) ((+) 1 1) (scanl (+) 1 fibs)
1 : 1 : scanl (+) (2) (scanl (+) ((+) 1 1) (scanl (+) 1 fibs))
1 : 1 : 2 : scanl (+) ((+) (2) ) (scanl (+) 2 (scanl (+) 1 fibs))


-}
fibsN x = fibs !! x

fibs20 = take 20 (1 : scanl (+) 1 fibs)
fibs100 = takeWhile (\x -> x < 100) (1 : scanl (+) 1 fibs)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- factorialScan = 1 : (foldl ((*) . (+1)) 1 factorialScan)
{-
factorialScan x = 1 : (scanl ((*) . (+1)) 1 factorialScan)
factorialScan x = 1 : 1 : scanl (*) ((*) 1 1) factorialScan
factorialScan x = 1 : 1 : scanl (*) ((*) 1 1) factorialScan

-}

{- CHAPTER EXERCISE
Warm-up and review
For the following set of exercises, you are not expected to use
folds. These are intended to review material from previous
chapters. Feel free to use any syntax or structure from previous
chapters that seems appropriate.
1. Given the following sets of consonants and vowels:
stops  = "pbtdkg"
vowels = "aeiou"
a) Write a function that takes inputs from stops and vowels
and makes 3-tuples of all possible stop-vowel-stop combinations.
These will not all correspond to real words in English, although
the stop-vowel-stop pattern is common enough that many of them
will.
b) Modify that function so that it only returns the combinations
that begin with a p.
c) Now set up lists of nouns and verbs (instead of stops
and vowels) and modify the function to make tuples
representing possible noun-verb-noun sentences.
-}
stops  = "pbtdkg"
vowels = "aeiou"
stopgostop xs ys = [[x,y,z] | x <- xs, y <- ys, z <- xs]
stopgostop1 xs ys = [[x,y,z] | x <- xs, y <- ys, z <- xs, x == 'p']

nouns = ["rock", "paper", "scissors"]
verbs = ["beats", "loses to", "ties with"]
stopgostop2 xs ys = [(x,y,z) | x <- xs, y <- ys, z <- xs]
{-
2.What does the following mystery function do? What is
its type? Try to get a good sense of what it does before
you test it in the REPL to verify it.
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc :: String -> Int
-- counts average count of letters per word
-}
seekritFunc x = div (sum (map length (words x))) (length (words x))

{-
3.We’d really like the answer to be more precise. Can you
rewrite that using fractional division?
-}
seekritFunc2 x = (/) (fromIntegral (sum (map length (words x)))) (fromIntegral (length (words x)))

{- Rewriting functions using folds
In the previous chapter, you wrote these functions using direct
recursion over lists. The goal now is to rewrite them using
folds.  Where possible, to gain a deeper understanding of
folding, try rewriting the fold version so that it is point-free.
Point-free versions of these functions written with a fold
should look like:
myFunc = foldr f z
So for example with the and function:
-- Again, this type will be less
-- reusable than the one in GHC 7.10
-- and newer. Don't worry.

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

-- direct recursion, using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

-- fold, not point-free
-- in the folding function
myAnd :: [Bool] -> Bool
myAnd = foldr
          (\a b ->
            if a == False
            then False
            else b) True
            
-- fold, both myAnd and the folding
-- function are point-free now
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

The goal here is to converge on the final version where
possible. You don’t need to write all variations for each
example, but the more variations you write, the deeper your
understanding of these functions will become.
1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr = undefined
-}
myOr1 :: [Bool] -> Bool
myOr1 [] = False
myOr1 (x:xs) = if x then True else myOr1 xs
myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs
myOr3 :: [Bool] -> Bool
myOr3 = foldr (\a b -> if a then True else b) False
myOr :: [Bool] -> Bool
myOr = foldr (||) False

{-

2. myAny returns True if a -> Bool applied to any of the values
in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined
Example for validating myAny:
Prelude> myAny even [1, 3, 5]
False
Prelude> myAny odd [1, 3, 5]
True
-}
myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 _ [] = False
myAny1 f (x:xs) = if f x then True else myAny1 f xs
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ [] = False
myAny2 f (x:xs) = f x || myAny2 f xs
myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f = foldr (\a b -> if f a then True else b) False
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = flip foldr False $ (||) . f

{-
3. Write two versions of myElem. One version should use
folding and the other should useany.
myElem :: Eq a => a -> [a] -> Bool
Prelude> myElem 1 [1..10]
True
Prelude> myElem 1 [2..10]
False
-}
myElem1 :: Eq a => a -> [a] -> Bool
myElem1 _ [] = False
myElem1 a (x:xs) = a == x || myElem1 a xs
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a = myAny (a ==)
myElem3 :: Eq a => a -> [a] -> Bool
myElem3 elem = foldr (\a b -> a == elem || b) False
myElem4 :: Eq a => a -> [a] -> Bool
myElem4 elem = flip foldr False ((||) . (elem ==))
myElem :: Eq a => a -> [a] -> Bool
myElem elem = flip foldr False $ (||) . (elem ==)

{-
4. Implement myReverse, don’t worry about trying to make
it lazy.
myReverse :: [a] -> [a]
myReverse = undefined
Prelude> myReverse "blah" "halb"
Prelude> myReverse [1..5] [5,4,3,2,1]
-}
myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 (x:xs) = myReverse1 xs ++ [x]
myReverse2 :: [a] -> [a]
myReverse2 = foldl (\b a -> a:b) []
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

{-
5. Write myMap in terms of foldr. It should have the same
behavior as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap = undefined
-}
myMap1 :: (a -> b) -> [a] -> [b]
myMap1 _ [] = []
myMap1 f (x:xs) = (f x) : (myMap1 f xs)
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr (\a b -> f a : b) []
-- myMap3 :: (a -> b) -> [a] -> [b]
-- myMap3 f = (flip foldl) [] ( (flip (:) .))
-- myMap3 :: (a -> b) -> [a] -> [b]
-- myMap3 = (flip foldr []) ((.) (flip (:)))

{-
6. Write myFilter in terms of foldr. It should have the same
behavior as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined
-}
myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 _ [] = []
myFilter1 f (x:xs) = if f x then x : myFilter1 f xs else myFilter1 f xs
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 f = foldr (\a b -> if f a then a : b else b) []

{-
7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = undefined
-}
squish1 :: [[a]] -> [a]
squish1 [] = []
squish1 (x:xs) = x ++ squish1 xs
squish2 :: [[a]] -> [a]
squish2 = foldr (\a b -> a ++ b) []
squish :: [[a]] -> [a]
squish = foldr (++) []

{-
8. squishMap maps a function over a list and concatenates the
results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined
Prelude> squishMap (\x -> [1, x, 3]) [2] [1,2,3]
Prelude> let f x = "WO " ++ [x] ++ " OT "
Prelude> squishMap f "blah"
"WO b OT WO l OT WO a OT WO h OT "
-}
squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 _ [] = []
squishMap1 f (x:xs) = (f x) ++ (squishMap1 f xs)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []
{-
9. squishAgain flattens a list of lists into a list. This time reuse
the squishMap function.f x
squishAgain :: [[a]] -> [a]
squishAgain = undefined
-}
squishAgain1 :: [[a]] -> [a]
squishAgain1 [] = []
squishAgain1 (x:xs) = x ++ squishAgain1 xs
squishAgain2 :: [[a]] -> [a]
squishAgain2 = foldr ((++) . id) []
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

{-
10. myMaximumBy takes a comparison function and a list and
returns the greatest element of the list based on the last
value that the comparison returned GTfor.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined
Prelude> myMaximumBy (\_ _ -> GT) [1..10]
1
Prelude> myMaximumBy (\_ _ -> LT) [1..10]
10
Prelude> myMaximumBy compare [1..10] 10
-}
-- myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy _ [] = 
-- myMaximumBy f (x:xs) = 

{-
11. myMinimumBy takes a comparison function and a list and
returns the least element of the list based on the last value
that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined
Prelude> myMinimumBy (\_ _ -> GT) [1..10]
10
Prelude> myMinimumBy (\_ _ -> LT) [1..10]
1
Prelude> myMinimumBy compare [1..10]
1
-}