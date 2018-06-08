module WordNumber where

import Data.List (intersperse, intercalate)

digitToWord :: Int -> String
digitToWord n
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"

digits :: Int -> [Int]
{- digits n = go n
    where
      leng = length (show n)
      divider = (^) 10 (leng -1)
      go n = divider -}

 -- (mod n 10) - retrieve the last digit
 -- (div n 10) - retrieve remaining digits minus the last one
digits n = go n []
    where
      go n result
        | ((div n 10) > 0) = go (div n 10) ((mod n 10) : result)
        | otherwise = n : result


wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))

