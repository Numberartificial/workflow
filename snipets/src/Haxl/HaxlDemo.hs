{-#LANGUAGE GADTs, EmptyDataDecls #-}
-- (the EmptyDataDecls pragma must also appear at the very top of the module,
-- in order to allow the Empty and NonEmpty datatype declarations.)

module HaxlDemo
       ()where

import Haxl.Core


-- |
-- >>> main
-- 42
main :: IO ()
main = do
    myEnv <- emptyEnv ()
    r <- runHaxl myEnv (return 42)
    print r

data DeepThought a where
    AnswerToLifeTheUniverseAndEverything :: DeepThought Int

runDeepThought :: DeepThought a -> ResultVar a -> IO ()
runDeepThought AnswerToLifeTheUniverseAndEverything var
  = putSuccess var 42
