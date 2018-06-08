{- 1 -}
tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10


-- (divMod) version
tensDigitDm :: Integral a => a -> a
tensDigitDm x = fst ((divMod) x 10)

hunsD x = d
    where d = fst ((divMod) x 100)

{- 2 -}
foldBool :: a -> a -> Bool -> a
foldBool x y b
    | b == True  = x
    | b == False = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b == True  = x
    | otherwise = y

{- 3 -}
g :: (a -> b) -> (a, c) -> (b, c)
g f (x,y) = (f(x), y)
