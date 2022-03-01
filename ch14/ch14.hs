{-
arbitrary :: Arbitrary a => Gen a
sample :: Show a => Gen a -> IO ()
sample' :: Gen a -> IO [a]


sample (arbitrary :: Gen Int)
sample (arbitrary :: Gen Double)

-- define a generator
trivialInt :: Get Int
trivialInt = return 1

-- return - function? return value in monad
return :: Monad m => a -> m a

return :: a -> Gen a
-}

