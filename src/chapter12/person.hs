type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == ""             = Left NameEmpty
  | otherwise              = Left AgeTooLow

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)
