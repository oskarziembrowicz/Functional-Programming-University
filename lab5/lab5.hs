-- foldl (+) x0 [x1, x2, x3, x4]
-- ((((x0 + x1) + x2) + x3) + x4)

-- foldr (+) x0 [x1, x2, x3, x4]
-- (x1 + (x2 + (x3 + (x4 + x0))))

-- replicate 3 4
-- [4, 4, 4]

-- takeWhile odd [1, 3, 5, 7, 2, 4, 9]
-- [1, 3, 5, 7]

-- dropWhile odd [1, 3, 5, 7, 2, 4, 9]
-- [2, 4, 9]

-- zip list1 list2
-- tworzy listę krotek

-- zipWith (/x y -> 10*x + y) [2, 3, 4] [7, 6, 5]
-- [27, 36, 45]

-- list !! n
-- !! - bierze element o indeksie n

-- ++ - konkatenacja list

-- data Maybe a = Nothing | Just a
--      deriving (Show, Eq)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h:t) = Just h

digitsToNumber :: Integral a => [a] -> a
digitsToNumber = foldl (\x y -> x*10 + y) 0

removeFromEnd :: Eq a => a -> [a] -> [a]
removeFromEnd el = foldr (removeHelper el) []

removeHelper :: Eq a => a -> a -> [a] -> [a]
removeHelper comp el list
    | el == comp && null list = []
    | otherwise = el:list

chainDivide :: Fractional a => [a] -> a
chainDivide [el] = el
chainDivide (h:t) = h + (1.0 / chainDivide t)

chainDivide2 :: Fractional a => [a] -> a
chainDivide2 = foldr1 (\el div -> el + 1 / div)

-- sumOfMaybe [Just 5, Nothing, Just 8] => 13
-- sumOfMaybe [] => 0
-- sumOfMaybe [Nothing, Nothing] => 0

-- maybeSumOfMaybe [Just 5, Just 8] => Just 13
-- maybeSymOfMaybe [Just 5, Nothing, Just 8] => Nothing
-- maybeSumOfMaybe [] = Just 0
-- maybeSumOfMaybe [Nothing, Nothing] => Nothing

maybeToNum :: Num a => Maybe a -> a
maybeToNum (Just a) = a
maybeToNum Nothing = 0

sumOfMaybe :: Num a => [Maybe a] -> a
-- sumOfMaybe [] = 0
-- sumOfMaybe (h:t) = maybeToNum h + sumOfMaybe t
sumOfMaybe list = sum (map maybeToNum list)

maybeSumOfMaybe :: Num a => [Maybe a] -> Maybe a
maybeSumOfMaybe [] = Just 0
maybeSumOfMaybe (Nothing:_) = Nothing
maybeSumOfMaybe (h:t) = sumMaybe h (maybeSumOfMaybe t)
-- maybeSumOfMaybe = foldl1 sumMaybe

sumMaybe (Just a) (Just b) = Just (a+b)
sumMaybe _ _ = Nothing