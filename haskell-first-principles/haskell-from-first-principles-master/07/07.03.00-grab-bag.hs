-- 1) all are quivalent
mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z


{-
2)
mTh1 3 is type of:
mTh1 3 :: Num a => a -> a -> a

mTh1 is type of:
mTh1 :: Num a => a -> a -> a -> a
-}

-- 3a
addOneIfOdd = \n -> case odd n of
  True -> f n
  False -> n
  where f n = n + 1

-- 3b
addFile = \x -> \y -> (if x > y then y else x) + 5

-- 3c
mflip f x y = f y x
