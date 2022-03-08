# Commands:
`stack ghci` - opens interactive environment or REPL
# Stack ghci commands: 
* `:q` | `:quit`    - quit
* `:i` | `:info`    - provides type info of operator or function
* `:l` | `:load`    - load file ex: `:load test.hs`
* `:m` | `:module`  - return to Prelude, unloads loaded file
* `:t` | `:type`    - find out type of value, expression, function
* `:s` | `:set`     - set REPL flag `:set -Wall`
* `import GHC.Int`  - imports Int to GHCi
* `:{ ... }:`       - block syntax in GHCi
* `:browse`         - list of loaded types and functions. can enter with module.
* `` | `` - 

# Quick basic syntax explanation
* `::`    - way to write type signature. ex:
* `$`     - allow everything on the right to evaluate first.
* `(+1)`  - sectioning. allow to pass around partially applied fn.
* `->`    - type constructor for function

# Snippets of how to write some usual code
* function composition, pipe
```haskell
-- (.) :: (b -> c) -> (a -> b) -> a -> c

let f x = negate . sum $ x
-- point free
let f' = negate . sum
-- f . g = \x -> f
```
* guard
```haskell
myAbs :: Integer -> Integer
myAbs x
  | x < 0       = (-x)
  | otherwise   = x
```
* case expression
```haskell
f x =
  case x + 1 == 1 of
    True -> "awesome"
    False -> "wut"
```
* if then else
```haskell
if x + 1 === 1 then "awesome" else "wut"
```
* `newtype`
```haskell
newtype Username = Username String
```
* function pattern matching
```haskell
-- simple data types 1
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
-- simple data types 2
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a,b) (c,d) = ((b, d), (a, c))
-- on custom data types
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer
data User = UnregisteredUser | RegisteredUser Username AccontNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrln "Unregistered"
printUser (RegisteredUser (Username name) (AccountNumber accNum)) =
  putStrLn $ name ++ " " ++ show accNum
```
* `do`    - special syntax that allows sequencing actions
```haskell
main :: IO()
main = do
  putStrLn "test"
  putStr "test"
```
* `++`    - concat list (like string) ex: `"test" ++ "ing"`
* defining function
   - must start with lowercase
```haskell
triple x = x * 3
-- name param = expression
```
* defining type signature
```haskell
sayHello :: String -> IO()
sayHello x = putStrLn ("Hello " ++ x)
```
* module names - start with capital letter
```haskell
-- sample.hs
module Sample where
x = 10
test = x * 5
```
* call function as infix: use backtick (`)
```haskell
10 `div` 2
```
* `let`   - introduces an expression.
```haskell
module FunctionWithWhere where
printInc2 n = let plusTwo = n + 2
in print plusTwo
```
* `where` - declaration. bound to surrounding syntactic construct.
```haskell
module FunctionWithLet where
printInc n = print plusTwo
where plusTwo = n + 2
```
* Data declaration - to define datatypes.
  * Type constructor - can only be used in type signatures.
  * Data constructor - values that inhabit type
  * always creates new type constructor may or may not always create new data constructor.
```haskell
data Bool = False | True
--   [1]     [2] [3] [4]
-- 1 Type constructor. name of type.
-- 2 data constructor for False value
-- 3 Pipe, indicates sum type (False *or* True)
-- 4 data constructor for True
-- whole line is the *data declaration*
```
* `type` - alias to refer to a type constructor or type constant
```haskell
type Name = String
```
* defining module
```haskell
module ModuleName where
-- module

import Data.Aeson (encode)
-- only import function `encode` 
import Database.Persist
-- import everything from Database.Persist
```
* Writing typeclass instance
```haskell
-- using `deriving`
data Trivial = Trivial deriving Eq
-- write own typeclass instance
data Trivial = Trivial'
instance Eq Trivial where
  Trivial' == Trivial' = True
-- for type with polymorphic parameter
data Identity a = Identity a
instance Eq a => (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
-- typeclass itself
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  -- ...
```


# List
* `[a]` polymorphic list.

# Miscellaneous
* `print` - accepts types with `Show` instance which is for human readable output
* `flip` - flip parameters of function
* `not :: Bool -> Bool` - negates `Bool`
* `:set -Wall` - so the compiler outputs some helpful error especially for partial function

# Terms
* Bottom - when incomplete pattern matching is called, unhandled cases return bottom. a non value that means cannot return value.
* arity - number of arguments a function accepts.
* parametrically polymorphic function
  * works for a value of any type
* constrained
  * polymorphic but is constrained/bounded to a set of types
  ```haskell
  isEqual :: Eq a => a -> a -> Bool
  isEqual x y = x == y
  -- accepts paramters that have instance of Eq typeclass
  ```
* ad-hoc polymorphism - applies typeclass constraint
```haskell
(+) :: Num a => a -> a -> a
-- applied Num constraint to type variable
```
* partial function - one that doesn't handle all possible cases like in typeclasses Eq implementation or functions
```haskell
data Test = Test1 | Test2
instance Eq Test where
  Test1 == Test1 = True
  Test2 == Test2 = True
-- does not handle all others like Test1 == Test2
f :: Int -> Bool
f 2 = True
-- does not handle other numbers aside 2
```

# Current Affairs
* reread current chapter: 8
* current chapter 14

# english vocabulary
* salient - most noticeable or important. pointing outward.
