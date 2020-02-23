-- ch7.hs

module Ch7 where

-- =============================
-- Anonymous functions
-- =============================

-- triple :: Integer -> Integer
-- triple x = x * 3
trip1 = (\x -> x * 3) :: Integer -> Integer

-- ====== TO USE IN GHCI ======
-- :{
--   let trip :: Integer -> Integer
--     trip = \x -> x*3   
-- :}

-- ====== TO APPLY ANONYMOUSLY ======
tripped5 = (\x -> x * 3) 5
-- 15

-- ====== Exercise ======
-- 1. Which (two or more) of the following are equivalent?
-- A: all of them
-- mTh x y z = x * y * z
-- mTh x y z = x * y * z
-- mTh x = \y -> \z -> x * y * z
-- mTh = \x -> \y -> \z -> x * y * z 

-- 2. The type of mTh (above) is Num a => a -> a -> a -> a. Which is the type of mTh 3?
-- A: last one
-- Integer -> Integer -> Integer
-- Num a => a -> a -> a -> a
-- Num a => a -> a
-- Num a => a -> a -> a

-- 3. Next, we’ll practice writing anonymous lambda syntax. For example, one could rewrite:
-- addOne x = x + 1
addOne = \x -> x + 1 

-- Try to make it so it can still be loaded as a top-level definition by GHCi.
-- This will make it easier to validate your answers.
-- a) Rewrite the f function in the where clause.
addOneIfOdd n = case odd n of 
              True -> f n
              False -> n
              -- where f n = n + 1
              where f = \n -> n + 1

-- b) Rewrite the following to use anonymous lambda syntax:
-- addFive x y = (if x > y then y else x) + 5 
addFive = \x -> \y -> (if x > y then y else x) + 5

-- c) Rewrite the following so that it doesn’t use anonymous lambda syntax:
-- mflip f = \x -> \y -> f y x
mFlip f x y = f y x





-- =============================
-- Pattern matching
-- can be used to unpack and expose content of data
-- and specify behavior depending on data
-- =============================

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

-- call `:set -Wall` to catch non exhaustive

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"
printUser (RegisteredUser
            (Username name)
            (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum



data WherePenguinsLive = 
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica deriving(Eq,Show)

data Penguin = Peng WherePenguinsLive deriving(Eq,Show)

-- ====== Pattern matching: union type ======

isSouthAfrica':: WherePenguinsLive-> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

-- ===== Pattern matching: unpacking =====

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True 
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p) || (antarcticPenguin p)

-- ===== Pattern matching: tuples =====

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))


-- ====== Exercise: Vareity pack ======
-- 1. Given the following declarations
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3, True)
-- a) What is the type of k?
-- ====== (a, b) -> a
-- b) What is the type of k2? Is it the same type as k1 or k3?
-- ===== string, No
-- c) Of k1, k2, k3, which will return the number 3 as the result?
-- ===== k1 and k3

-- 2. Fill in the definition of the following function:
-- Remember: Tuples have the same syntax for their type constructors and their data constructors.
f' ::(a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f' (a, _, c) (d, _, f) = ((a, d), (c, f))


-- ===============================
-- Case Expressions
-- ===============================

-- data Bool = False | True
--      [1]    [2]     [3]

-- 1.Type constructor, we only use this in type signatures, notin term-level code like case expressions.
-- 2.Data constructor for the value of Bool named False — we can match on this.
-- 3.Data constructor for the value of Bool named True — we can match on this as well.

-- if x + 1 == 1 then "AWESOME" else "wut"
funcZ x = case x + 1 == 1 of
            True -> "AWESOME"
            False -> "wut"


pal' xs = case y of
            True -> "yes"
            False -> "no"
          where y = xs == reverse xs

-- ====== Exercise: Case Practice ======
-- 1. The following should return x when x is greater than y.
functionC x y = if(x > y) then x else y
functionC' x y = case x > y of
                  True -> x
                  _ -> y
-- 2. The following will add 2 to even numbers and otherwise simply return the input value.
ifEvenAdd2 n = if even n then (n + 2) else n
ifEvenAdd2' n = case even n of
                  True -> n + 2
                  _ -> n
-- The next exercise doesn’t have all the cases covered.
-- See if you can fix it.
-- 3. The following compares a value, x, to zero and returns an indicator for whether x is a postive number or negative number. 
-- But what if x is 0? You may need to play with the compare function a bit to find what to do.
nums x = case compare x 0 of
            LT -> -1
            GT -> 1
            EQ -> 0

-- ============================
-- Higher Order Functions (HOF)
-- ============================

-- Prelude> :t flip
-- flip :: (a -> b -> c) -> b -> a -> c
--           [   1   ]      [     2     ]
-- 1 - function argument. note the parenthesis
-- 2 - other arguments and result

-- ====== Exercise: Artful dodgy ======

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

dodgyTest = [
  (dodgy 1 1) == 11,
  (dodgy 2 2) == 22,
  (dodgy 1 2) == 21,
  (dodgy 2 1) == 12,
  (oneIsOne 1) == 11,
  (oneIsOne 2) == 21,
  (oneIsTwo 1) == 21,
  (oneIsTwo 2) == 22,
  (oneIsOne 3) == 31,
  (oneIsTwo 3) == 23
  ]


-- =============================
-- Guards Blocks
-- =============================
  
myAbs :: Integer -> Integer
myAbs x
--[1] [2]
  | x < 0     = (-x)
--[3] [4]    [5] [6]
  | otherwise = x
--  [7]

-- 1 - function name
-- 2 - parameter
-- 3 - pipe | to begin a guard case
-- 4 - expression to test if this branch should evaluate. must evaluate to Bool.
-- 5 - denotes that we are declaring what expression to return if guard is true.
-- 6 - expression to return. (if true)
-- 7 - another name for True


-- ====== Exercise: Guard Duty ======

-- 1. What is the type of pal?
-- 2. The following function returns?
pal :: Eq a => [a] -> Bool -- answer 1
pal xs
  | xs == reverse xs = True
  | otherwise        = False

-- answer 2: True of palindrome
palTest = pal "racecar"

-- 3. the following function returns?
-- 4. what is the type of numbers?
numbers :: (Ord a, Num a, Num p) => a -> p
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- answer 3: an indication of whether its argument is a positive ornegative number or zero



-- =============================
-- Function Composition
-- =============================
-- (.) :: (b -> c) -> (a -> b) -> a -> c
--        [1]         [2]     [3]  [4]
-- 1. is a function from 𝑏 to 𝑐, passed as an argument (thus the parentheses).
-- 2. is a function from 𝑎 to 𝑏.
-- 3. is a value of type 𝑎, the same as [2] expects as an argument.
-- 4. is a value of type 𝑐, the same as [1] returns as a result.

-- Then with the addition of one set of parentheses:
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
--        [1]         [2]         [3]
-- 1. given a function 𝑏 to 𝑐
-- 2. given a function 𝑎 to 𝑏
-- 3. return a function 𝑎 to 𝑐.

testComp = negate . sum $ [1, 2, 3, 4, 5]
-- evaluates like so
-- negate . sum $ [1, 2, 3, 4, 5]
-- negate (sum [1, 2, 3, 4, 5])
-- negate (15)
-- -15

-- uses $ operator so that `negate . sum` will evaluate first instead `sum [1,2,3,4,5]`
-- this happens because a normal function has precendence of 10 while (.) has 9 (sum has higher)
-- you can also just use parenthesis



-- =============================
-- Pointfree style
-- =============================
-- Refers to composing functions without specifying their arguments

-- let f = negate . sum
-- f [1, 2, 3, 4, 5]
-- -15


-- =============================
-- Demonstrating Composition
-- =============================
-- putStrLn :: String -> IO ()
-- show :: Show a => a -> String

-- here is composition of  print
print :: Show a => a -> IO ()
print = putStrLn . show



-- =============================
-- Chapter Exercise
-- =============================
{-|
  1. A polymorphic function
  Answer: d
  a) changes things into sheep when invoked
  b) has multiple arguments
  c) has a concrete type
  d) may resolve to values of different types, depending on inputs

  2. Two functions named f and g have types Char -> String and String -> [String] respectively.
     The composed function g . f has the type
     Answer: b
  a) Char -> String
  b) Char -> [String]
  c) [[String]]
  d) Char -> String -> [String]
  
  3. A function f has the type Ord a => a -> a -> Bool and we apply it to one numeric value. 
     What is the type now?
     Answer: d
  a) Ord a => a -> Bool 
  b) Num -> Num -> Bool
  c) Ord a => a -> a -> Integer
  d) (Ord a, Num a) => a -> Bool

  4. A function with the type (a -> b) -> c
     Answer: b
  a) requires values of three different types
  b) is a higher-order function
  c) must take a tuple as its first argument
  d) has its parameters in alphabetical order

  5. Given the following definition of f, what is the type of f True?
     f :: a -> a 
     f x = x
     Answer: a
  a) f True :: Bool
  b) f True :: String
  c) f True :: Bool -> Bool
  d) f True :: a  
-}

{- Let's Write Code
tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div`10
          d     = xLast `mod` 10
-}

-- rewrite in divMod version
tensDigit' x = d
  where xLast = fst . divMod x $ 10
        d     = snd . divMod xLast $ 10
-- does the divMod version have the same type? - YES
-- hundreds version
hunsD x = d
  where xLast = fst . divMod x $ 100
        d     = snd . divMod xLast $ 10

-- implement in case expression and guard
-- foldBool :: a -> a -> Bool -> a
-- foldBool =

foldBoolC a b c = case c of
  True -> a
  otherwise -> b

foldBoolG a b c
  | c = a
  | otherwise = b

-- implement
g :: (a -> b) -> (a, c) -> (b, c)
g x (a, c) = (x a, c)