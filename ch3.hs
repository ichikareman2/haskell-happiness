-- ch3.hs
module Ch3 where


addExclamation :: String -> String
addExclamation x = x ++ "!"

get5th :: String -> String
get5th x = [head $ drop 4 x]

drop9 :: String -> String
drop9 x = drop 9 x

get3rd :: [a] -> a
get3rd x = x !! 2

letterIndex :: Int -> Char
letterIndex i = (!!) text $ i - 1
  where text = "Curry is awesome!"

rvrs :: String -> String
rvrs x = concat [c, " ", b, " ", a, " "]
  where a = take 5 x
        b = drop 6 $ take 8 x
        c = drop 9 x