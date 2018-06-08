{- 1 -}
{-
dividedBy 15 2 == 15 - 2, 13
                     - 2, 11
                     - 2, 9
                     - 2, 7
                     - 2, 5
                     - 2, 3
                     - 2 1-}
-- stop - remainder: 1

{- 2 -}
addOneToN :: (Eq a, Num a) => a -> a
-- addOneToN n = addN 0 0 n
    -- where addN iteration accumulator max
    --       | (iteration >= max) accumulator
    --       | otherwise (iteration > max) addN (iteration + 1) (accumulator + iteration) max

addOneToN n = addN 1 0 n
  where addN iteration accumulator max
          | (iteration == max + 1) = accumulator
          | otherwise = addN (iteration + 1) (accumulator + iteration) max


{- 3 -}
recSum :: (Integral a) => a -> a -> a
recSum a b = sum 0 0 a b
  where sum iteration accumulator max multiplier
          | (iteration == max) = accumulator
          | otherwise = sum (iteration +1) (accumulator + multiplier) max multiplier
