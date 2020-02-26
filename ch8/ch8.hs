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

-- dividedBy :: Integral a => a -> a -> a
-- dividedBy num denom = go num denom 0
--   where go n d count
--     | n < d = (count, n)
--     | otherwise = go (n - d) d (count + 1)

-- =============================
-- Chapter Exercise
-- =============================

-- Review Currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha" 
--1.What is the value ofappedCatty "woohoo!"? Try to deter-mine the answer for yourself, then test in the REPL.2.frappe "1"3.frappe (appedCatty "2")4.appedCatty (frappe "blue")5.cattyConny (frappe "pink")(cattyConny "green" (appedCatty "blue"))6.cattyConny (flippy "Pugs" "are") "awesome"