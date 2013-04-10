

test :: a [] Int -> Bool
test = error "a"


test2 :: a Maybe Int -> Bool
test2 = error "a"

data T a b = T (a b)
-- data T a b = T a b 


fmap :: (a -> b) -> f a -> f b
fmap = error "a"

test' = test (T [1])

test3 :: T [] Int
test3 = error "a"

test4 :: T X Int
test4 = error "a"

test5 :: T () Int
test5 = error "a"

test5 :: T (,) Int
test5 = error "a"

test6 :: T (,,) Int
test6 = error "a"

test7 :: T (,,,) Int
test7 = error "a"

test8 :: T (->) Int
test8 = error "a"

test9 :: a (->) Int
test9 = error "a"


