
data A0 a = A0 a

data A1 a = A1 a
  deriving ()


data A2 a = A2 a
  deriving Eq

data A3 a = A3 a
  deriving (Eq)

data A4 a = A4 a
  deriving (Eq, Ord)

data A5 a = A5 a
  deriving (Eq, Ord, Show)


newtype A6 a = A6 a
 
newtype A7 a = A7 a
  deriving ()

newtype A8 a = A8 a
  deriving Eq

newtype A9 a = A9 a
  deriving (Eq)

newtype A10 a = A10 a
  deriving (Eq, Ord)

newtype A11 a = A11 a
  deriving (Eq, Ord, Show)


{-
type X a = X a
  deriving (Eq)

 -}