-- database-processing.hs
module DatabaseProcessing where
-- Exercises: Database Processing

import Data.Time
data DatabaseItem = DbString String | DbNumber Integer| DbDate UTCTime deriving (Eq, Ord, Show)
theDatabase :: [ DatabaseItem ]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr fn [] xs
                  where
                    fn (DbDate a) b = a:b
                    fn _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr fn [] xs
                    where
                      fn (DbNumber n) b = n:b
                      fn _ b = b
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr fn def xs
                  where
                    fn (DbDate a) b = if a > b then a else b
                    fn _ b = b
                    def = (UTCTime
                      (fromGregorian 1900 0 0)
                      (secondsToDiffTime 0))

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr fn 0 xs
            where
              fn (DbNumber a) b = a + b
              fn _ b = b

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sum filtered) / fromIntegral (length filtered)
            where
              filtered = foldr fn [] xs
              fn (DbNumber a) b = a:b
              fn _ b = b