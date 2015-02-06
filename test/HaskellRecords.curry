module M (D (C1, C2)) where

data D = C1 Int
       | C2 String
       | C3 Bool

-- data
-- should be parsed
-- data RD = RD {}
-- data RD = RD { x,y,z :: Int, a :: Bool, r :: RD }
-- data RD a = RD { f :: a }


-- newtype
-- should be parsed
-- newtype RN = RN { x :: Int }

-- should NOT be parsed
-- newtype RN = RN { x,y :: Int }
-- newtype RN = RN { x :: Int, y :: Bool }
-- newtype RN = RN { }

-- record construction
-- r1 = R1 { x = 12, y = False }
-- r2 = R2 { }
-- r3 = R3 { x = 42,  r = r3 }

-- record selection
-- i = x r3

-- record update
-- should be parsed
-- r3' = r3 { x = 24, y = 72 }
-- r3' = (r r3) { x = 24, y = 72 }

-- r3' = (r3 { })

-- record pattern

-- f R1 { x = 45 } = True
-- f R1 { x = 45, y = False } = True
-- f R1 { } = True
