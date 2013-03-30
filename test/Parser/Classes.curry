
-- ---------------------------------------------------------------------------
-- Class declarations, without where
-- ---------------------------------------------------------------------------

class Eq a

-- class Eq =>

class () => Eq b

class Eq a => Eq b

class (Eq a) => Eq b

class (Eq a, Eq b) => Eq c

class (Eq a, Eq b, Eq c) => Eq c

class (Eq a, Eq b, Ord c, Eq d) => Eq c

class Abc.Def a => Eq b

class (Abc.Def a) => Eq b

class (Abc.Def a, Def.Geh b) => Eq b

class (Abc.Def a, Def.Geh b, Abc e) => Eq b

class (Abc a, Def.Geh b, Abc e) => Eq b

class (Abc a, Def.Geh b, Abc.Def.Geh.Abc.Def.Geh e) => Eq b

-- ---------------------------------------------------------------------------
-- Class declarations, with where
-- ---------------------------------------------------------------------------

class Eq a where

class Eq a where
  a :: b -> c

class Eq a where
  a :: b -> c
  b :: b -> c
  c :: b -> c

class Eq a where
  a, x, y :: b -> c

class Eq a where
  a = 1
 
class Eq a where
  a = 1
  b = 2
  c = 3

class Eq a where
  a :: d -> e
  a = 1
  
class Eq a where
  a = 1
  a :: d -> e

class Eq a where
  a :: b -> e
  a = 1
  b :: x -> y
  b = 2

class Eq a where
  a = 2
    + 3
    + 4

class Eq a where
  a = 4
    where d = f

x = 1

class Eq a where
  a = 1

z = 1 + x

class Eq a where
  -- (a, b) = (1, 2)
  a `op` b = 1
  b + c = 2


class Eq a where
  a :: Int -> Int
  b = 2
  b = 1
  b = 10
  