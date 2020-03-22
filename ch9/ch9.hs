-- ch9.hs
module Ch9 where

-- =============================
--
--             Lists
--
-- =============================

{-
data []  a   =    []  |   a : [a]
--  [1] [2] [3]  [4] [5]   [6]
1. The datatype with the type constructor[]
2. takes a single type constructor argument ‘a’
3. at the term level can be constructed via
4. nullary constructor []
5. or it can be constructed by
6. data constructor (:) which is a product of a value of the type a
   we mentioned in the type constructor and a value of type [a], that is, “more list.”

The cons constructor (:) is an infix data constructor and goes between the two arguments 𝑎 and [a]
that it accepts. Since it takes two arguments, it is a product of those two arguments, like the 
tuple type (a, b). Unlike a tuple, however, this constructor is recursive because it mentions its
own type [a] as one of the members of the product.
-}


-- =============================
-- Pattern matching on lists
-- =============================

{-
let myHead (x:_) = x
-- :t myHead
-- [a] -> a
myHead [1, 2, 3]
-- 1

let myTail (_:xs) = xs
-- :t myTail
-- [a] -> [a]
myTail [1, 2, 3]
-- [2, 3]

-- Doesn't handle empty list.
-- if we try to pass them an empty list as an argument, they can’t pattern match:

Prelude> myHead []
*** Exception:
  Non-exhaustive patterns
  in function myHead
Prelude> myTail []
*** Exception:
  Non-exhaustive patterns
  in function myTail

-- One way of fixing is putting a base case:

myTail :: [a] -> [a]
myTail [] = []
myTail (_ :xs) = xs

-- A better way is to use `Maybe` datatype (discussed fully in later chapter).
-- The idea is to make your failure case explicit. As programs get longer and more complex that can be quite useful.

Prelude> :info Maybe
data Maybe a = Nothing | Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

-- try to make safeHead yourself:
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just a

-}  

-- =============================
-- List’s syntactic sugar
-- =============================

{-
Haskell has some syntactic sugar to accommodate the use of lists, so that you can write:

Prelude> [1, 2, 3] ++ [4]
[1, 2, 3, 4]

Rather than:

Prelude> (1 : 2 : 3 : []) ++ 4 : []
[1, 2, 3, 4]

-}

-- =============================
-- Using ranges to construct lists
-- =============================
-- There are several ways we can construct lists.  One of thesimplest is with ranges.
{-
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]

Prelude> enumFromTo 1 10
[1,2,3,4,5,6,7,8,9,10]

Prelude> [1,2..10]
[1,2,3,4,5,6,7,8,9,10]

Prelude> enumFromThenTo 1 2 10
[1,2,3,4,5,6,7,8,9,10]

Prelude> [1,3..10]
[1,3,5,7,9]

Prelude> enumFromThenTo 1 3 10
[1,3,5,7,9]

Prelude> [2,4..10]
[2,4,6,8,10]

Prelude> enumFromThenTo 2 4 10
[2,4,6,8,10]

Prelude> ['t'..'z']
"tuvwxyz"

Prelude> enumFromTo 't' 'z'
"tuvwxyz"
-}

-- =============================
-- Exercise: EnumFromTo
-- =============================
{-
Some things you’ll want to know about the Enum typeclass:
Prelude> :info Enum
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
Write your own enumFromTo definitions for the types provided. 
Do not use range syntax to do so. It should return the same 
results as if you did [start..stop].
-}
eFromTo :: (Eq a,  Enum a) => a -> a -> [a]
eFromTo x y = go x (y:[])
  where go first arr
          | head arr == first = arr
          | otherwise = go first $ (pred . head $ arr) : arr

eftBool :: Bool ->  Bool -> [Bool]
eftBool = eFromTo

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eFromTo

eftInt :: Int -> Int -> [Int]
eftInt = eFromTo

eftChar :: Char -> Char -> [Char]
eftChar = eFromTo

-- =============================
-- Extracting portions of lists
-- =============================
{-
-- takes the specified number of items off the beginning of list
take :: Int -> [a] -> [a]
-- drops specified number of items at the beginning of list
drop :: Int -> [a] -> [a]
-- cuts list into two parts at the element specified
splitAt :: Int -> [a] -> ([a], [a])
-- a HOF. Takes/Drops items from the beginning of list as long as
-- it meets the condition
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
-}

-- =============================
-- Exercises: Thy Fearful Symmetry
-- =============================
{-
1. Using takeWhile and dropWhile, write a function that takes a
  string and returns a list of strings, using spaces to separate
  the elements of the string into words, as in the following
  sample:
Prelude> myWords "sheryl wants fun"
["wallfish", "wants", "fun"]
-}



