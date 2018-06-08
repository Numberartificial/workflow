eftBool :: Bool -> Bool -> [Bool]
eftBool x y = enumFromTo x y

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = enumFromTo x y

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
  where
    go x y result
      | x > y = []
      | x < y = go (succ x) y (result ++ [x])
      | x == y = (result ++ [x])

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
  where
    go x y result
      | x > y = []
      | x < y = go (succ x) y (result ++ [x])
      | x == y = (result ++ [x])
