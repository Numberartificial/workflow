chk :: Eq b => (a -> b) -> a -> b -> Bool
chk a b c = False
{-
alternative solution found on github:
chk f x y = f(x) == y
-}

arith :: Num b => (a -> b) -> Integer -> a -> b
arith a b c = 1
{-
alternative solution found on github:
arith f x y = f(y) + fromIntegral(x)
-}
