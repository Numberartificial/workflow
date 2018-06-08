pal xs
    | xs == reverse xs = True
    | otherwise = False

{-
3. b) -
  "aba" - returns True
  "abb" - returns False
4. it takes Eq type class arguments, reverse takes lists like "aba" or [1,2,1]
5. pal :: Eq a => [a] -> Bool
6.
-}


numbers x
  | x <  0 = -1
  | x == 0 =  0
  | x >  0 =  1

{-
6. c)
7. Ord and Num
8. numbers :: (Ord a, Num a, Num t) => a -> t
-}
