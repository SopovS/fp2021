-- Лабораторна робота №2
    -- Студента групи КН-31 підгрупа 2
    -- Сопов Сергій 
    -- Варіант №7

    -- Мета: Набути досвiду визначення рекурсивних функцiй, використання механiзму зiставлення зi зразком i роботи з кортежами та списками.


--Замiнити кожен n-й елемент списку вказаним значенням, напр. при
n=2 та значеннi ’z’: "1234590"⇒ "1z3z5z0".

f1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
let zipped = zip f1 [1..]
 zipped
[(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9),(10,10),(11,11),(12,12),(13,13),(14,14),(15,15)]
replace_every_k f1, k, rep = map (\(value, index) -> if index `rem` 3 == 0 then value*2 else value) zipped

<interactive>:10:20: error: parse error on input ‘,’
 replace_every_k f1 k rep = map (\(value, index) -> if index `rem` k == 0 then rep else value) f1


--  Знайти найбiльший спiльний дiльник двох чисел.

f2 :: Int -> Int -> Int
nsd 0 y = y
f2 x y = if x>y then f2 (x-y) y
                 else f2 (y-x) x
f21 :: Int -> Int -> Int
f21 0 y = y
f21 x y = if x>y then f21 (x-y) y
                  else f21 (y-x) x

--Висновки:Була проведена робота із рекурсивними функцiями із використанням механiзму зiставлення та набуті навички в роботі із картежами.