silnia 0 = 1
silnia n = n * silnia (n-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fib' 0 = 0
fib' n = fibIter n 1 0 1

fibIter n count prev f =
    if count == n then f
    else fibIter n (count + 1) f (prev+f)

sin' x = sinIter x 1 x x

sinIter x i a s = 
    if (abs a) < 0.0000001 then s
    else
        let
            next = (-1) * a * x^2 / ((i+1) * (i+2))
        in
        sinIter x (i+2) next (s+next)

cos' x = sinIter x 0 1 1

f x = x^2 - 4
f' x = 2*x

bisection f a b = 
    let
        c = (a+b)/2
    in
    if (f c) == 0 then c
    else
        if abs (a-b) < 0.0000001 then c
        else
            if f a * f c < 0 then bisection f a c
            else bisection f c b