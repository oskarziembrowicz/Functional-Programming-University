{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
empty' _ = []

odd_numbers :: Integral a => [a]
odd_numbers = even_numbers_hlp 1

even_numbers :: Integral a => [a]
even_numbers = 0 : even_numbers_hlp 2
even_numbers_hlp n = n : (-n) : even_numbers_hlp (n+2)

contains :: Eq a => [a] -> a -> Bool
contains = flip elem

add :: Eq a => a -> [a] -> [a]
add x s = x : remove x s

remove_dup :: Eq a => [a] -> [a]
remove_dup [] = []
remove_dup (h:t) = h : remove h (remove_dup t)

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove el (h:t)
    | h == el    = remove el t
    | otherwise  = h : remove el t

sum' :: Eq a => [a] -> [a] -> [a]
-- sum' s1 s2 = remove_dup (s1 ++ s2)
sum' s1 (h:t) = sum' (add h s1) t

intersection :: Eq a => [a] -> [a] -> [a]
intersection s1 s2 = filter (contains s1) s2