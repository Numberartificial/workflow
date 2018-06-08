{-
definitions:
DayOfWeek is a datatype
Mon, Tue are data instances
-}

-- intro:
data DayOfWeek = Mon|Tue|Wed|Thu|Fri|Sat|Sun deriving (Show)

data Date =
  Date DayOfWeek Int deriving (Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
      weekday == weekday' && dayOfMonth == dayOfMonth'

-- 1
data TisAnInteger = TisAn Integer deriving (Show)
instance Eq TisAnInteger where
  (==) (TisAn num)
       (TisAn num') =
        num == num'

-- 2
data TwoIntegers = Two Integer Integer deriving (Show)
instance Eq TwoIntegers where
  (==) (Two numOne numTwo)
       (Two numOne' numTwo') =
          (==) numOne numOne' && (==) numTwo numTwo'

-- 3
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt num) (TisAnInt num')     = num == num'
  (==) (TisAString str) (TisAString str') = str == str'
  (==) _ _                                = False

-- 4
data Pair a = Pair a a deriving (Show)
instance Eq a => Eq (Pair a) where
  (==) (Pair one two) (Pair one' two') =
    (==) one one' && (==) two two'

-- 5
data Tuple a b = Tuple a b deriving (Show)
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') =
    (==) a a' && (==) b b'

-- 6
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne one) (ThisOne one') = one == one'
  (==) (ThatOne one) (ThatOne one') = one == one'
  (==) _ _                          = False

-- 7
data EitherOr a b = Hello a | Goodbye b deriving (Show)
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v')    = v == v'
  (==) (Goodbye v) (Goodbye v') = v == v'
  (==) _ _                     = False
