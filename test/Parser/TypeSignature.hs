
test :: a [] Int -> Bool
test = error "a"

test2 :: a Maybe Int -> Bool
test2 = error "a"

data T a b = T (a b)

fmap :: (a -> b) -> f a -> f b
fmap = error "a"

test' = test (T [1])
