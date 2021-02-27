-- ch11.hs
module Ch11 where
import Data.Char
import Data.List
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
        newIndex = (fromMaybe 0 keyIndex) + (fromMaybe 0 charIndex)
        getIndex i = (mod (i) (charLimit))
        go = case charIndex of
            Just a -> chars !! getIndex newIndex
            Nothing -> char

getCharIndex :: Char -> Maybe Int
getCharIndex c = elemIndex char chars
    where char = toLower c

cipherTest = cipher "ally" "meet at dawn"


{- As patterns-}
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf (a:as) bb = if elem a bb
    then isSubseqOf as (dropWhile (\x -> x /= a) bb)
    else False

isSubseqOfTest = all id [
    isSubseqOf "blah" "blahwoot",
    isSubseqOf "blah" "wootblah",
    isSubseqOf "blah" "wboloath",
    not $ isSubseqOf "blah" "wootbla",
    not $ isSubseqOf "blah" "halbwoot",
    isSubseqOf "blah" "blawhoot"
    ]

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords s = (word, capitalWord) : (capitalizeWords (dropSpace . dropFirstWord $ s))
    where
        word = takeWhile (\x -> x /= ' ') s
        dropFirstWord s = dropWhile (\x -> x /= ' ') s
        dropSpace s = dropWhile (\x -> x == ' ') s
        capitalWord = (toUpper . head $ word) : (tail word)

capitalizeWordsTest = capitalizeWords "hello world"

{- Language exercise -}
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (s:ss) = if s == ' ' then s : capitalizeWord ss else toUpper s : ss

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph s = (formatSentence . takeSentence $ s) ++ capitalizeParagraph (dropPeriod . dropSentence $ s)
    where
        takeSentence = takeWhile (\x -> x /= '.')
        dropSentence = dropWhile (\x -> x /= '.')
        dropPeriod = dropWhile (\x -> x == '.')
        formatSentence = capitalizeWord . (++ ".")

trim :: String -> String
trim = reverse . removeLeadingSpace . reverse . removeLeadingSpace
    where removeLeadingSpace = dropWhile (\x -> x == ' ')

capitalizeParagraphTest = capitalizeParagraph "blah. woot ha"

data DaPhone = DaPhone [KeyIn] deriving (Eq, Show)
data KeyIn = KeyIn {
        key :: PhoneKey,
        taps :: Int
    } deriving (Eq, Show)
data PhoneKey = Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | Key0 | KeyAsk | KeyHash deriving (Eq , Show)

convo :: [String]
convo = ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

-- reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
-- reverseTaps :: Da