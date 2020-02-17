-- mood.hs

module Mood where

data Mood = Blah | Woot deriving Show

instance Eq Mood where
  Blah == Woot = False
  Woot == Blah = False
  _ == _ = True

-- changeMood x = if (x == Blah) then Woot else Blah
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah