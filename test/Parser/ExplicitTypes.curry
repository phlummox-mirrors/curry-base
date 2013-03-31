
a :: Int -> Int

a = ((5 :: Int -> a) + (3 :: Bool) + (2 :: Eq a => Int) `div` 2) :: Eq b => Int
a = ((5 :: Int -> a) + (3 :: Bool) + (2 :: Eq a => Int) `div` 2) :: Int


x :: Bool
x = 5
x = 5 :: Int
x = 5 :: () => Int
x = 5 :: Eq a => Int
x = 5 :: (Eq a) => Int
x = 5 :: (Eq a, Ord b) => Int
x = 5 :: (Eq a, Ord (b c)) => Int


x = (5 + (5 + ((5 :: Eq a => Int) + (5 :: Int))))
