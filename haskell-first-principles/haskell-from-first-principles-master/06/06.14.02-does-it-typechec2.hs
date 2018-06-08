data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                  then Blah
                  else x
