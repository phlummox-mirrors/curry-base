

a :: Int

b :: Bool -> a -> [a] -> () -> (a,b) -> Tree x -> Abc.Def

c :: Eq a => a
d :: () => a
e :: (Eq a) => a
f :: (Eq a, Ord b) => a

g :: Eq (a b) => a
h :: (Eq (a b)) => a
i :: (Eq (a b), Eq (a Int Bool (Int -> Int) ([a] -> [c]))) => a


j1 :: Eq (a Bool) => b
j2 :: Eq (a Prelude.Bool) => b
j3 :: Eq (a k) => b
j4 :: Eq (a k l) => b
j5 :: Eq (a k l m) => b
j6 :: Eq (a []) => b
j7 :: Eq (a ()) => b
j8 :: Eq (a (->)) => b
j9 :: Eq (a (,)) => b
j10 :: Eq (a (,,)) => b
j11 :: Eq (a (,,,)) => b

j12 :: Eq (a ()) => b
j13 :: Eq (a (c)) => b
j14 :: Eq (a (c,d)) => b
j15 :: Eq (a (c,d,e)) => b

j16 :: Eq (a [e]) => b
j17 :: Eq (a [e] [f]) => b
j18 :: Eq (a [e] [f] (c,d) () [x]) => b

k :: (Eq (a e), Ord (b Int)) => b
l :: (Eq a, Ord (b Int)) => a