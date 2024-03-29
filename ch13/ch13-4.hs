type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)
mkPerson :: Name
    -> Age
    -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 =
        Right $ Person name age
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Input name"
    name <- getLine
    putStrLn "Input age"
    age <- getLine
    let person = mkPerson name (read age)
    case person of
      Right (Person name age) -> do print person
      Left NameEmpty -> do putStrLn "Name is empty" 
      Left AgeTooLow -> do putStrLn "Age too low" 
      Left (PersonInvalidUnknown str) -> do putStrLn str 

