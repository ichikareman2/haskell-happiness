-- ch7.hs

module Ch7 where

-- ====================
-- Anonymous functions
-- ====================

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





-- ====================
-- Pattern matching
-- can be used to unpack and expose content of data
-- and specify behavior depending on data
-- ====================

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
