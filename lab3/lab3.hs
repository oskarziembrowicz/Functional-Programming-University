import Language.Haskell.TH (safe)

empty _ = False

oddNumbers :: Integer -> Bool
oddNumbers = odd

evenNumbers :: Integer -> Bool
evenNumbers = even

contains :: (a -> Bool) -> (a -> Bool)
contains s = s
-- contains = id
-- id - funkcja indentycznościowa

add :: Eq a => (a -> Bool) -> a -> a -> Bool
add s elem x = s x || x == elem

remove :: Eq t => (t -> Bool) -> t -> t -> Bool
remove s elem x = x /= elem && s x

sumSets :: (t -> Bool) -> (t -> Bool) -> t -> Bool
sumSets s1 s2 x = s1 x || s2 x

intersection :: (t -> Bool) -> (t -> Bool) -> t -> Bool
intersection s1 s2 x = s1 x && s2 x

difference :: (t -> Bool) -> (t -> Bool) -> t -> Bool
difference s1 s2 x = s1 x && not (s2 x)

complement :: (t -> Bool) -> t -> Bool
complement s = not . s

-- flip odwraca kolejność argumentów w funkcji"

listToPredicate :: Eq a => [a] -> a -> Bool
listToPredicate = flip elem