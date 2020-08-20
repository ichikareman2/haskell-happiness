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
foldr (++) "" ["woot", "WOOT", "woot"]
foldr max '' "fear is the little death"
and [False, True]
foldr (||) False [False ,True]
foldr flip const 'a' [1..5]
foldl const 0 "tacos"
foldl (const) 0 "burritos"
foldl (const) 'z' [1..5]