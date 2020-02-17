-- ch5.hs
module Ch5 where

-- (++) :: [a] -> [a] -> [a]
myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"

-- (*) :: Num a => a -> a -> a
myMult :: Fractional a => a -> a
myMult x = (x / 3) * 5

-- take :: Int -> [a] -> [a]
myTake :: Int -> [Char]
myTake x = take x "hey you"

-- (>) :: Ord a => a -> a -> Bool
myCom :: Int -> Bool
myCom x = x > (length[1..10])

-- (<) :: Ord a => a -> a -> Bool
myAlph :: Char -> Bool
myAlph x = x < 'z'