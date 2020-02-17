-- ch4.hs
module Ch4 where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a == reverse a

myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1) else x

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

x = (+)
F xs = w 'x' 1
  where w = length xs