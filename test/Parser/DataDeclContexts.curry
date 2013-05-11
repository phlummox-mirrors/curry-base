

data T a b = T a b

data C a => T a b = T a b

data C a => T a b = T a b | S a b

data (C1 a, C2 a) => T a b = T a b

newtype T a b = T a

newtype C a => T a b = T a

newtype (C1 a, C2 a) => T a b = T a

