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