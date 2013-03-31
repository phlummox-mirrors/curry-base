

class F a where
  f :: a (d e) e -> d e -> Bool

test :: (F a, Eq (a [c] ())) => c -> Bool
test = undefined

class F2 f where
  f2 :: f a -> Bool

test2 :: (F2 a) => a Bool
test2 = undefined

class F3 a where
  f3 :: a d e -> d e -> Bool

test3 :: (F3 a, Eq (a [] Bool)) => a [] Int -> Bool
test3 = undefined

test4 :: a [] Int -> Bool
test4 = undefined

data Test a b = Test (a b)

test5 = test4 (Test [1])

