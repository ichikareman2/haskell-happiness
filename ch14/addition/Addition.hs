-- Addition.hs
module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO()
main = hspec $ do
  describe "Addition" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
    it "5 multiplied by 3 is 15" $ do
      multiplyBy 5 3 `shouldBe` 15
    it "4 multiplied by 5 is 20" $ do
      multiplyBy 4 5 `shouldBe` 20
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2) `shouldBe` 4

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
            go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy a b = go a b 0
  where go n1 n2 t
           | 0 == n2 = t
           | otherwise = go n1 (n2 - 1) (t + n1)

sayHello :: IO ()
sayHello = putStrLn "hello!"
{-
choose :: System.Random.Random a => (a, a) -> Gen a
elements :: [a] -> Gen a
frequency :: [(Int, Gen a)] -> Gen a
-}
-- defining a generator
trivialInt :: Gen Int
trivialInt = return 1
-- run `sample trivialInt`

-- different generator
oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]
-- run `sample' oneThroughThree`

--different generator
genBool :: Gen Bool
genBool = choose (True, False)

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)
-- call `sample (genTuple :: Gen (Int, Float))` ,
-- call `sample (genTuple :: Gen ([()], Char))`
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]
-- run `sample (genEither :: Gen (Either Int Bool))`

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
-- run `sample (genMaybe :: Gen (Maybe Int))`

-- QuickCheck without Hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO()
runQc = quickCheck prop_additionGreater