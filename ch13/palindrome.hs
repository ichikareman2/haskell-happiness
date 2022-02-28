import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if cleanString line1 == reverse (cleanString line1)
      then putStrLn "It's a palindrome!"
      else do putStrLn "Nope!"
              exitSuccess

cleanString :: String -> String
cleanString = filter isAlpha . map toLower