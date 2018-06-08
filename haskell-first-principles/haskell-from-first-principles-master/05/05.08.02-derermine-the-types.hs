{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineType where

x = 5
y = x + 5
w :: Num a => a
w = y * 10

x2 = 5
y2 = x2 + 5
z :: Num a => a -> a
z y2 = y2 * 10

x3 = 5
y3 = x3 + 5
f :: Fractional a => a
f = 4 / y3

x4 = "Julie"
y4 = "<3"
z4 = "Haskell"
f2 :: [Char]
f2 = x4 ++ y4 ++ z4
