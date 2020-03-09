-- ch8.hs
module Ch8 where

-- =============================
-- Recursion
-- =============================

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-=========================
  Step by step evaluation
  =========================

  factorial 4 = 
    4 * factorial (4 - 1)
    4 * factorial 3
    4 * 3 * factorial (3 - 1)
    4 * 3 * factorial 2
    4 * 3 * 2 * factorial (2 - 1)
    4 * 3 * 2 * factorial 1
    4 * 3 * 2 * 1 * factorial (1-1)
    4 * 3 * 2 * 1 * 1
    24
-}

-- =============================
-- Another way to look at it
-- =============================

inc :: Num a => a -> a
inc = (+1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

{-
  In a function such as this, the looming threat of unending
  recursion is minimized because the number of times to apply
  the function is an argument to the function itself, and we’ve
  defined a stopping point: when (times - 1) is equal to zero, it
  returns 𝑛 and that’s all the applications we get. We can 
  abstract the recursion out of incTimes, too:
-}

applyTimes :: (Eq a,Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
-- applyTimes n f b = f (applyTimes (n - 1) f b)
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n


-- =============================
-- Bottom
-- =============================

{-
  ⊥ or bottom is a term used in Haskell to refer to computations that do not successfully result in a value
-}
f :: Bool -> Int
f True = error "blah"
f False = 0

f' :: Bool -> Int
f' False = 0


-- data Maybe a = Nothing | Just a
{-
  The Maybe datatype can take an argument. In the first case,
  Nothing, there is no argument; this is our way to say that there
  is no result or data from the function without hitting bottom.
  The second case, Just a takes an argument and allows us to
  return the data we’re wanting. Maybe makes all uses of nil values
  and most uses of bottom unnecessary. Here’s how we’d use it with 𝑓
-}
-- f :: Bool -> Maybe Int
-- f False = Just 0
-- f _ = Nothing


-- =============================
-- Fibonacci
-- =============================

fibonacci :: Integral a => a -> a 
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

-- =============================
-- Integral division from scratch
-- =============================

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

-- =============================
-- Chapter Exercise
-- =============================

-- Review Currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny
appedCatty :: String -> String
appedCatty = cattyConny "woops"
frappe :: String -> String
frappe = flippy "haha" 
-- What is the value of the following
-- 1. appedCatty "woohoo!"? 
-- 2. frappe "1"
-- 3. frappe (appedCatty "2")
-- 4. appedCatty (frappe "blue")
-- 5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
-- 6. cattyConny (flippy "Pugs" "are") "awesome"
curryTest = [
  ((appedCatty "woohoo!") == "woops"++" mrow "++"woohoo!" ),
  frappe "1" == "1 mrow haha",
  frappe (appedCatty "2") == "woops mrow 2 mrow haha",
  appedCatty (frappe "blue") == "woops mrow blue mrow haha",
  cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) == "pink mrow haha mrow green mrow woops mrow blue",
  cattyConny (flippy "Pugs" "are") "awesome" == "are mrow Pugs mrow awesome"
  ]

-- Recursion
-- 1. Write out the steps for reducing dividedBy 15 2 to its final answer according to the Haskell code.
{-
dividedBy 15 2
go 15 2 0
go 13 2 1
go 11 2 2
go 9 2 3
go 7 2 4
go 5 2 5
go 3 2 6
go 1 2 7
(7, 1)
-}
-- 2. Write a function that recursively sums all numbers from 1 to n, n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 + 5 to get 15. 
-- The type should be (Eq a, Num a) => a -> a.
sumFrom1 x = go x 0
  where go x total
          | x == 0 = total
          | otherwise = go (x - 1) 0 + x
-- 3.Write a function that multiplies two integral numbers using recursive summation. The type should be (Integral a) => a -> a -> a.
recSum :: (Integral a) => a -> a -> a
recSum x y = go x y 0
  where go x y total
          | y == 0 = total
          | otherwise = go x (y - 1) total + x

-- Fixing dividedBy: make `dividedBy` be able to receive negative and 0 (0 should be like maybe)
data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' 0 _ = DividedByZero
dividedBy' _ 0 = DividedByZero
dividedBy' num denom = if shouldBeNegative then Result $ negate goRun else Result goRun
  where go n d count
         | n < d = count
         | otherwise = go (n - d) d (count + 1)
        shouldBeNegative = (num < 0 && denom > 0) || (denom < 0 && num > 0)
        goRun = go (abs num) (abs denom) 0


{- McCarthy 91 function
We’re going to describe a function in English, then in math
notation, then show you what your function should return for
some test inputs. Your task is to write the function in Haskell.
The McCarthy 91 function yields 𝑥 − 10 when 𝑥 > 100 and 91 otherwise.
The function is recursive.
𝑀𝐶(𝑛)=⎧{⎨{⎩𝑛−10if𝑛>100𝑀𝐶(𝑀𝐶(𝑛+11))if𝑛≤100mc91=undefined
You haven’t seen map yet, but all you need to know right
now is that it applies a function to each member of a list and
returns the resulting list. It’ll be explained in more detail in
the next chapter.
Prelude> map mc91 [95..110]

[91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
-}
mc91 x
  | x > 100 = x - 10
  | otherwise = 91

mcTest = map mc91 [95.. 110]
