
-- zipWitch (+) [1,2,3] [10,11,12]  ----> [11,13,15]

--sum(map sum [[1,3,4,5],[4,6,3,4]]) sumy tych macierzy wszystkiego

-- replicate 3 6 czyli [6,6,6]
--setEl (h:t) 0 val = val:t
--setEl (h:t) n val = h : (setEl (n-1) t)



poprawnaMacierz :: [[a]] -> Bool
poprawnaMacierz [] = False
poprawnaMacierz ([]:_) = False
poprawnaMacierz (h:t) =
    let
        lKol =length h
    in
        --all (\wiersz -> length wiersz == lKol) t
        all ((==lKol).length) t

elementMacierzy :: [[a]] -> Int -> Int -> a
elementMacierzy mac wiersz kol = (mac !! wiersz) !! kol

sumaElementow :: Num a => [[a]] -> a
--sumaElementow mac = sum (map sum mac)
sumaElementow = sum.(map sum)

macierzStala :: Num a => a -> Int -> Int -> [[a]]
macierzStala wartosc m n = replicate m (replicate n wartosc)

macierzZerowa :: Num a => Int -> Int -> [[a]]
--macierzZerowa m n = macierzStala 0 m n
macierzZerowa = macierzStala 0 

wierszMacjedn :: Num a => Int -> Int -> [a]
wierszMacjedn lkol w = replicate w 0 ++ [1] ++ replicate (lkol -w -1) 0
--wierszMacjedn lkol w = map (\i -> if i== w then 1 else 0) [0..lkol-1]

macierzJednostkowa :: Num a => Int -> [[a]]
macierzJednostkowa n = map (wierszMacjedn n)[0..n-1]

ustawWartosc (h:t) 0 val = val:t
ustawWartosc (h:t) n val = h : (ustawWartosc t (n-1) val)
--              n
-- m[[1,2,3],[3,4,5],[6,7,8]] ----> [3,40,5] chcemy zmienic 4 na 40 w macierzy n na m
ustawWartoscMac :: a ->Int -> Int -> [[a]] -> [[a]]
ustawWartoscMac wart m n mac = 
    ustawWartosc mac m (ustawWartosc (mac !! m) n wart)

kolumna :: Int -> [[a]] -> [a]
kolumna n = map (!!n)

przekatna :: [[a]] -> [a]
--przekatna = zipWith (\i w -> w !! i) [0..] mac
--przekatna = zipWith(flip (!!)) [0..]
przekatna = zipWith (!!) mac [0..]

sumaMacierzy :: Num a => [[a]]-> [[a]]->[[a]] 
sumaMacierzy = zipWith (zipWith (+))