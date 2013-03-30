
-- ---------------------------------------------------------------------------
-- Instance declarations
-- ---------------------------------------------------------------------------

instance Eq Int

instance Eq a => Eq Int

instance (Eq a) => Eq Int

instance (Eq a, Eq b) => Eq Int

instance () => Eq Int

instance Eq Abc.Def.Int

instance Eq ()

instance Eq []

instance Eq (->)

instance Eq (,)

instance Eq (,,)

instance Eq (,,,)





instance Eq (Tree)

instance Eq (Tree a)

instance Eq (Tree a b)

instance Eq (Tree a b c)

instance Eq (Abc.Tree a b c d)

instance Eq (() a b c)

instance Eq ([])

instance Eq ([] a)

instance Eq ((->))

instance Eq ((->) a)

instance Eq ((->) a b)

instance Eq ((->) a b c)

instance Eq ((,))

instance Eq ((,) a)

instance Eq ((,) a b)

instance Eq ((,) a b c)

instance Eq ((,,,))

instance Eq ((,,,) a)

instance Eq ((,,,) a b)

instance Eq ((,,,) a b c)

instance Eq ((,,,) a b c d)

instance Eq ((,,,) a b c d e)




instance Eq (a, b)

instance Eq (a, b, c)

instance Eq (a, b, c, d)

instance Eq [a]

instance Eq (a -> b)



instance Eq a where

instance Eq a where
  x = 5
  y = z
  -- a :: Int-> Int
  -- a external

instance Eq a where
  x = 1

instance Eq a where
  x = y
  z = d
  e = f

instance Eq a where
  a `op` b = c
  a 1 2 3 = e
  a + c = 2

