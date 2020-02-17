
-- ch6.hs
module Ch6 where
import Data.List

data Trivial = Trivial'
-- [1] [2]     [3]
-- [1] - keyword, begins data declaration
-- [2] - type constructor, name of type, shows up in type signatures
-- [3] - data constructor

instance Eq Trivial where
-- [1]   [2] [3]    [4]
  Trivial' == Trivial' = True
  -- [5]   [6] [7]       [8]
-- (==) Trivial' Trivial' = True
-- [              9              ]


-- [1] - keyword, begins declaration of typeclass instance
--     - typeclass instance are how you tell Haskell how Typeclass function should work for this data type

-- [2] - The Typeclass you are implementing

-- [3] - The Type you are providing for. You are implementing Eq for Trivial

-- [4] - terminates begining of instance

-- [5] - data declaration (value)

-- [6] - the function you are defining

-- [7] - data declaration (value)

-- [8] - The result

-- [9] - alternate way of defining the function

-- for polymorphic parameter, add constraint like in functions
data Identity a = Identity a deriving Show
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'


data TisAnInteger = Tis Integer deriving Show
instance Eq TisAnInteger where
  (==) (Tis i1) (Tis i2) = i1 == i2

data TwoInteger = Two Integer Integer deriving Show
instance Eq TwoInteger where
  (==) (Two i1 i2) (Two i1' i2') = (i1 == i1' && i2 == i2')

data StringOrInt = TisAnInt Int | TisAString String deriving Show
instance Eq StringOrInt where
  (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False


data Pair a = Pair a a  deriving Show
instance Eq a => Eq (Pair a) where
  (==) (Pair a1 a2) (Pair a1' a2') = a1 == a1' && a2 == a2'
  -- (==) _ _ = False

data Tuple a b = Tuple a b deriving Show
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a deriving Show
instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b deriving Show
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False



-- 5
ex1 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
-- LT
ex2 = compare (3 * 4) (3 * 5)
-- ERROR
-- ex3 = compare "Julie" True
-- False
ex4 = (5 + 3) > (3 + 6)


data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = if x == Woot 
                then Blah
                else x

type Subject = String
type Verb = String
type Object = String

data Sentence = 
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"



data Rocks = Rocks String deriving (Eq, Show, Ord)
data Yeah = Yeah Bool deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'





i :: Num a => a
-- i :: a
-- error, no class or instance(?) arising from a
i = 1

f :: Float
-- f :: Num a => a
-- error, can't go up a level in type/class (?)
f = 1.0

-- f' :: Float
f' :: Fractional a => a
f' = 1.0


-- f'' :: Float
f'' :: RealFrac a => a
-- TODO:: Search what is Real and RealFrac
f'' = 1.0

-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
-- sigmund :: a -> a
-- error, can't return Int concrete type when forcing 
-- input and output to be same type and be parametric
sigmund x = myX

-- myX = 1 :: Int
sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
-- error, same as above, just not as high since
-- parametric type is the top class(?)
sigmund' x = myX

-- You’ll need to import sort from Data.List.
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
-- error, even if signifier is Ord, a typeclass
-- it is already forced to be for [Char] since
-- it mySort, is used inside which is for [Char] only
signifier xs = head (mySort xs)





chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn a b = fn a == b

-- test:
-- xx x = 2
-- chk xx 'a' 2
-- True
-- chk xx 'a' 3
-- False

-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith fn i a = fn a

-- test:
-- arith id 3 2
-- 2
-- arith (2+) 3 2
-- 4
-- arith (4.0+ 3 2
-- 6.0
