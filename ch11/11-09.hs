-- 11.9 newtype
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

{- 
    newtype
 -}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (a,_) = a > 42

instance TooMany (Int, Int) where
    tooMany (a, b) = a + b > 42

instance (Ord a, Num a, TooMany a) => TooMany (a, a) where
    tooMany (a, b) = a + b > 42

newtype Goats a = Goats a deriving (Eq, Show, TooMany)


-- instance 