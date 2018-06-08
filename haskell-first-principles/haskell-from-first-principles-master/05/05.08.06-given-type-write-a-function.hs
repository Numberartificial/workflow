module GivenTypeWriteAFunction where

-- 1
i :: a -> a
i a = a

-- 2
c :: a -> b -> a
c a _ = a

-- 3
-- same thing as a function c
c'' :: b -> a -> b
c'' b _ = b

-- 4
c' :: a -> b -> b
c' _ b = b

-- 5
r :: [a] -> [a]
r a = a
-- or
r' :: [a] -> [a]
r' a = tail a

-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = (bToC (aToB a))

-- 7
a :: (a -> c) -> a -> a
a aToC a = a

-- 8
a' :: (a -> b) -> a -> b
a' aToB a = aToB a
