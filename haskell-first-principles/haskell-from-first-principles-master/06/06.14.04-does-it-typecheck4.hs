type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

-- this won't work because Object argument is not applied
s1 = Sentence "dogs" "drool"

-- this will work because all 3 arguments were applied
s2 = Sentence "Julie" "loves" "dogs"

