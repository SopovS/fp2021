--Лабораторна робота №1
--студента групи кн-31
--Сопова Сергія
--Варіант 9

--Мета
--Ознайомитись з модульною органiзацiєю програм та засобами введення-виведення. Набути досвiду компiляцiї Haskell-програм.


--Завдання. 
--Вставити у список через кожнi n елементiв вказане значення, напр. через n=2 значення ’z’: "1234590"⇒ "12z34z59z0".

--а)

prompt :: (Read a) => String -> IO a
prompt s = (putStr s >> getLine) Data.Functor.<&> read


insert n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs -- reset to original n
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs


main :: IO ()
main = do

-- -- З клавіатури
    n <- prompt "The number? "
    c <- prompt "The char?"
    l <- prompt "The line? "

-- -- На екран
    let insertRes = insert n c l
    putStrLn $ "The insert result " ++ insertRes

-- -- З файлу
--     handle <- openFile "input.txt" ReadMode 
--     l <- hGetLine handle
--     str <- hGetLine handle
--     let n = read str :: Int
--     hClose handle

-- -- В файл
--     let res = insert n c l
--     let f = fst res
--     let s = snd res
--     let res_final = f ++ "\n" ++ s
--     writeFile "output.txt" res_final
--     putStr "Result is in the file output.txt :) "
