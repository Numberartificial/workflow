module Sing2 where

fstString :: [Char] -> [Char]
fstString x = x ++ " river"

sndString :: [Char] -> [Char]
sndString x = x ++ " than a mile"

sing = if (x > y) then fstString x
  else sndString y
    where x = "Moon"
          y = "wider"
