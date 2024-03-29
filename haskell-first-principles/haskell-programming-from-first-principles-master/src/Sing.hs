module Sing where

fstString :: String -> String
fstString x = x ++ " in the rain"

sndString :: String -> String
sndString x = x ++ " over the rainbow"

sing :: String
sing      = if x > y then fstString x else sndString y
  where x = "Singing"
        y = "Somewhere"
