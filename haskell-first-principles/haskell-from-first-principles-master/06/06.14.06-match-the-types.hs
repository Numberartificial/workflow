import Data.List

-- 1
-- No instance for (Num a) arising from the literal ‘1’
-- i :: a
i :: Num a => a
i = 1

-- 2
e :: Float
-- This doesn't work because it should be 'Fractional a'
-- e :: Num a => a
e = 1.0


-- 3
-- This works because Fractional accepts 1.0, and it's an instance of the Float type
f :: Fractional a => a
-- f :: Float
f = 1.0

-- 4
-- This works because RealFrac accepts 1.0, and it's an instance of the Float type
g :: RealFrac a => a
-- g :: Float
g = 1.0

-- 5
-- This works because you can impose type constraints on a function argument
freud :: Ord a => a -> a
-- freud :: a -> a
freud x = x

-- 6
-- This works - function argument is limited to Int and has to return Int
freud' :: Int -> Int
-- freud' :: a -> a
freud' x = x

-- 7
myX = 1 :: Int
-- This won't work because Int doesn't match 'a'
-- sigmund :: a -> a
sigmund :: Int -> Int
sigmund x = myX

-- 8
myX' = 1 :: Int
-- sigmund' :: Int -> Int
-- This won't work because we're passing Int
-- sigmund' :: Num a => a -> a
-- This works
sigmund' :: Int -> Int
sigmund' x = myX'

-- 9
-- jung :: Ord a => [a] -> a
-- This works because we just limit the list items to be type of Int
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10
-- young :: [Char] -> Char
-- This works because we've got a data constraint of Ord, which allows sorting (it's got > and <)
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- This doesn't work because signifier is passed [Char]
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)
