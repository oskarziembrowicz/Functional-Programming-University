-- foldl (+) x0 [x1, x2, x3, x4]
-- ((((x0 + x1) + x2) + x3) + x4)

-- foldr (+) x0 [x1, x2, x3, x4]
-- (x1 + (x2 + (x3 + (x4 + x0))))
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

f a = a : f a

-- ścisła ewaluacja
-- head (f 5)
-- head (5 : f(5))
-- head (5 : (5 : (f 5)))

head' (h:_) = h

-- leniwa ewaluacja
-- head (f 5)
-- head (5 : (f 5))
-- 5

nat = nat_hlp 1
nat_hlp s = s : nat_hlp (s+1)