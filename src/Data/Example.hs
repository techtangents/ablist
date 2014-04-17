
infixr 5 :/
data ABList a b = ABNil | a :/ ABList b a
  deriving (Eq, Ord, Show)

reverse :: ABList a b -> Either (ABList a b) (ABList b a)
reverse ABNil = Right ABNil
reverse (a :/ ABNil) = Left ABNil
reverse (a :/ b :/ ABNil) = Right $ b :/ a :/ ABNil
reverse (a :/ b :/ c :/ ABNil) = Left $ c :/ b :/ a :/ ABNil
reverse (a :/ cs) = Z
