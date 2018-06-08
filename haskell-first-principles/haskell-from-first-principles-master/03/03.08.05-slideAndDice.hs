module SliceAndDice where

rvrs :: String -> String
rvrs text = (take  7 (drop 9 text) ++ " " ++ drop  6 (take 8 text)) ++ " " ++ (take 5 text)
