f x = x + 1

add x y = x + y

add5 = add 5

g 0 = 0
g _ = 1

h 0 0 = 0
h 0 x = x + 1
h _ _ = 10

empty [] = True
empty _ = False

listHead (h : _) = h

last' [h] = h
last' (_:t) = last' t

length' [] = 0
length' (_:t) = 1 + length' t

sumOfPower a b = a * a + b * b

powerOfSum a b = (a + b) * (a + b)

sumOfMultiplications a b c d = a * b + c * d

f4 a b c d = (a - b) / (c + d)

f5 a b c = a / b * c

f6 a = floor (a / 4) 

f7 a b = (a / b) ^ 3

f8 a b = sqrt(abs(a - b))

ex2 a =
    if a == 1 then "złoty"
    else
        if floor (a / 10) == 1 then "złotych"
        else if mod a 10 == 