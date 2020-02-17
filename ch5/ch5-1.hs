{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- Num
-- a = (* 9) 6
-- Num a => (a, [Char])
-- b = head [(0,"doge"),(1,"kitteh")]
-- (Integer, [Char])
-- c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
-- Bool
-- d = if False then True else False
-- Int
-- e = length [1, 2, 3, 4, 5]
-- Bool
-- f = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- w :: Num a => a
-- x = 5
-- y = x + 5
-- w = y * 10

-- x = 5
-- y = x + 5
-- z y = y * 10
-- What is the type of z?
-- z :: Num a => a -> a

-- x = 5
-- y = x + 5
-- z = 4 / y
-- What is the type of z?
-- z :: Fractional a => a

-- x = "Julie"
-- y = " <3 "
-- z = "Haskell"
-- g = x ++ y ++ z 
-- What is the type of g?
-- g :: [Char]

bigNum = (^) 5 $ 10 
wahoo = (bigNum, 10)

x = print
y = print "woohoo!"
z = x "hello world"

-- a = (+)
-- b = 5
-- c = a b 10
-- d = a c 200

-- c = 0
-- b = 10000 * c
-- a = 12 + b

-- functionH :: [a] -> a
functionH (x:_) = x

-- functionC :: Ord a => a -> a -> Bool
functionC x y = 
  if (x > y) then True else False

--functionS :: (a, b) -> b
functionS (x, y) = y

i :: a  -> a
i a = a

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' a b = a

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r a = reverse a

co :: (b -> c) -> (a -> b) -> a -> c
co f1 f2 a = f1 $ f2 a

a :: (a -> c) -> a -> a
a f a = a

a' :: (a -> b) -> a -> b
a' f a = f a