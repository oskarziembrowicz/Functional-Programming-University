import Language.Haskell.TH.Ppr (bytesToString)
data Fraction = Frac Integer Integer

(//) :: Integer -> Integer -> Fraction
a // b = norm (Frac a b)
infixl 7 //     -- operator :/ łączny lewostronnie z priorytetem 7

reduce :: Fraction -> Fraction
reduce (Frac p q) = 
    let
        gcd_pq = gcd p q
    in
        Frac (div p gcd_pq) (div q gcd_pq)

norm :: Fraction -> Fraction
norm (Frac _ 0) = error "divided by zero"
norm (Frac 0 _) = Frac 0 1
norm (Frac p q)
    | q < 0 = reduce (Frac (-p) (-q))
    | otherwise = reduce (Frac p q)

instance Num Fraction where
    fromInteger n = n // 1
    abs (Frac p q) = (abs p) // q
    negate (Frac p q) = (-p) // q
    signum (Frac 0 _) = Frac 0 1
    signum (Frac p q)
        | signum p == signum q = Frac 1 1
        | otherwise = Frac (-1) 1
    (*) (Frac p1 q1) (Frac p2 q2) = (p1 * p2) // (q1 * q2)
    (+) (Frac p1 q1) (Frac p2 q2) = (p1*q2 + p2*q1) // (q1 * q2)
    (-) (Frac p1 q1) (Frac p2 q2) = (p1*q2 - p2*q1) // (q1 * q2)

instance Eq Fraction where
    (==) f1 f2 =
        let
            (Frac p1 q1) = norm f1
            (Frac p2 q2) = norm f2
        in
            (p1 == p2) && (q1 == q2)

instance Ord Fraction where
    (<=) f1 f2 =
        let
            (Frac p1 q1) = norm f1
            (Frac p2 q2) = norm f2
        in
            p1*q2 <= p2*q1

instance Show Fraction where
    show f = 
        let
            Frac p1 q1 = norm f
        in
            show p1 ++ "/" ++ show q1