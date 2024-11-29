-- [i^2 | i <- [0..10]]
-- zwróci: [0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- [i^2 | i <- [0..10], odd i]
-- zwróci: [1, 9, 25, 49, 81]

macierzZFunkcji :: (Int -> Int -> a) -> Int -> Int -> [[a]]
macierzZFunkcji f m n = [[f i j | j <- [0..n-1]] | i <- [0..m-1]]