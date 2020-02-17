sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

-- triple :: Number -> Number
triple x = x * 3
square x = x * x
getCircleArea radius = pi * square radius