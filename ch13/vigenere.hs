import Data.List
import Data.Char
import Data.Maybe

{- Vigenere Cipher -}
cipher :: String -> String -> String
cipher key str = map (\x -> (shift (fst x) (snd x))) keyCharPair
    where
        keyStr = createKey key str
        keyCharPair = zip keyStr str

createKey :: String -> String -> String
createKey _ [] = []
createKey (k:ks) (s:ss) = k : createKey nextKey ss
    where nextKey = if elem s chars then ks ++ [k] else k:ks

chars :: String
chars = ['a'..'z']
charLimit :: Int
charLimit = length chars

shift :: Char -> Char -> Char
shift key char = go
    where
        keyIndex = getCharIndex key
        charIndex = getCharIndex char
        newIndex = fromMaybe 0 keyIndex + fromMaybe 0 charIndex
        getIndex i = mod i charLimit
        go = case charIndex of
            Just a -> chars !! getIndex newIndex
            Nothing -> char

getCharIndex :: Char -> Maybe Int
getCharIndex c = elemIndex char chars
    where char = toLower c

runApp :: IO()
runApp = do
    putStrLn "Input key string"
    key <- getLine
    putStrLn "Input lint to shift"
    line <- getLine
    case (all isAlpha key, all isAlpha line) of
      (False, _) -> putStrLn "key must be alphabet characters"
      (_, False) -> putStrLn "key must be alphabet characters"
      (True, True) -> putStrLn $ cipher key line