--Лабораторна робота №1
--студента групи кн-31
--Сопова Сергія
--Варіант 9

--Мета
--Набути досвiду визначення та використання функцiй вищого порядку.

--Завдання 1. Вставити у список через кожнi n елементiв вказане значення, напр. через n=2 значення ’z’: "1234590"⇒ "12z34z59z0".
--а)
insert :: Int -> a -> [a] -> [a]
insert n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs -- reset to original n
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs

--б)
insertAtN :: Int -> a -> [a] -> [a]
insertAtN n y  = intercalate [y] . groups n
  where
    groups n = takeWhile (not.null) . unfoldr (Just . splitAt n)
    

--Завдання 2. Знайти перше просте число в указаному дiапазонi.
--а)
nPrimes x y =  head [z | z <- [x..y], isPrime z]
isPrime :: (Integral a) => a -> Bool
isPrime k | k <=1 = False | otherwise = 0 `notElem`  moding k [2..k-1]

moding _ [] = []
moding k (x:xs) = mod k x: moding k xs


--б) 
--nPrimes :: Integer -> Integer -> Integer 
nPrimes2 x y =  head [z | z <- [x..y], isPrime z]

isPrime2 :: (Integral a) => a -> Bool
isPrime2 k | k <=1 = False | otherwise = 0 `notElem` map (mod k)[2..k-1]
