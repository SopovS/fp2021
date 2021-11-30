module Main where
    -- Лабораторна робота №3
    -- Студента групи КН-31 підгрупа 2
    -- Сопов Сергій 

-- Функція, повторююча n-кратно кожен елемент списку.

rep1 n lst = map (:[]) (concatMap (\x -> pov n x) lst)
pov n x = mypov n x []
mypov 0 x l = l
mypov n x l = l++x : mypov (n-1) x l

-- Функцiя визначає Знайти простi дiльники числа

primes = f [2..]
where f (p:xs) = p : f [x | x <- xs, mod x p /= 0]

primeFactors2 x = f x (head primes) (tail primes)
where f x n ns
| x < 2         = []
| x < n ^ 2     = [x]
| mod x n == 0  = n : f (div x n) n ns
| otherwise     = f x (head ns) (tail ns)

.