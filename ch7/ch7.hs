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



