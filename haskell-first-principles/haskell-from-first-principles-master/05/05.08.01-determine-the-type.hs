{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineType where

example = 1

a :: Num a => a
a = (* 9) 6
-- returns 54

b :: Num t => (t, [Char])
b = head [(0,"doge"),(1,"kitteh")]
-- reutrns (0, "dodge")

c :: (Integer, [Char])
c =  head [(0 :: Integer ,"doge"),(1,"kitteh")]
-- returns (0 "dodge")

d :: Bool
d = if False then True else False
-- returns False

e :: Int
e = length [1, 2, 3, 4, 5]
-- returns 5

f :: Bool
f = (length [1, 2, 3, 4]) > (length "TACOCAT")
-- returns False
