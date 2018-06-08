module TypeVarOrTypeConstructor where

f :: Num a => a -> b -> Int -> Int
             [0]  [1]   [2]    [3]
-- 0 constrained polymorphic (Num)
-- 1 fully polymorphic
-- 2 concrete type constructor
-- 3 concrete type constructor

f :: zed -> Zed -> Blah
     [0]    [1]    [2]
-- 0 fully polymorphic
-- 1 concrete type polymorphic
-- 2 concrete type polymorphic

f :: Enum b => a -> b -> C
              [1]  [2]  [3]
-- 1 fully polymorphic
-- 2 constrained polymorphic
-- 3 concrete type

f :: f -> g -> C
    [0]  [1]  [2]
-- 0 fully polymorphic
-- 1 fully polymorphic
-- 2 concrete type
