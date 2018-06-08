data Rocks =
  Rocks String deriving (Eq, Ord, Show)

data Yeah =
  Yeah Bool deriving (Eq, Ord, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Ord, Show)

-- this will not typecheck
-- phew = Papu "chases" True
-- this will typecheck
phew = Papu (Rocks "chases") (Yeah True)

-- this will typecheck
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

-- this will typecheck
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- this didn't typecheck because Papu, Rocks, Yeah didn't derive Ord
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
