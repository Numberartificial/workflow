module MonadPlus
       ()where

import Control.Monad


lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM _ [] = mzero
lookupM k ((x,y):xys)
  | x == k = return y `mplus` lookupM k xys
  | otherwise = lookupM k xys

x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
