-- arith4.hs
module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
    print (roundTrip 4)
    print (id 4)
    print (roundTripPointFree 4)
    print ((roundTrip 4) :: Int)


roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree a = (read . show) a

-- in order to make it work you need to specify the actual output type
roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 a = (read . show) a

