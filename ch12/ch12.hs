-- ch12.hs
module Ch12 where
import Data.Char
import Data.List
import Data.Maybe

id :: a -> a
id = undefined
r :: a -> f a
r = undefined

notThe :: String -> Maybe String
notThe s 
  | (map toLower s) == "the" = Nothing
  | otherwise = Just s

{-

| head arr == first = arr
| otherwise = go first $ (pred . head $ arr) : arr
-}
replaceIfNothing :: Maybe -> String
replaceIfNothing Just a = a
replaceIfNothing Nothing = "a"

replaceThe :: String -> Maybe String
replaceThe str = go "" str
  where go str 