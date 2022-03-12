import Lib
import Test.Hspec
import Data.Monoid
main :: IO ()
main = hspec $ do
  describe "Optional mappend" $ do
    it "returns Only Sum 2 for (Only Sum 1) and (Only Sum 1)" $ do
      (Only (Sum 2)) == (mappend (Only (Sum 1)) (Only (Sum 1)))
    it "returns Only Product 8 for (Only Product 4) and (Only Product 2)" $ do
      (Only (Product 8)) == (Only (Product 4)) `mappend` (Only (Product 2))
    -- it "returns Only Sum 1 for (Only Sum 1) and (Nada)" $ do
    --   ()

