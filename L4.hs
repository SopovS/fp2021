
type Author = String
type Name = String
type City = String
type Year = Int
type Number = Int
type Pages = Int
type Source = String

data Publication = Book Author Name City Source Year |  
               Article Author Name Source Year Number Pages |
               Thesus Author Name Source City Year Pages
               deriving Show




showPublish:: [Publication] -> [Source]
showPublish [] = []
showPublish ( Book _ _ _ [] _:xs)  = showPublish xs 
showPublish ( Book _ _ _ n _ :xs) = isAlreadyExists n (showPublish xs)

showPublish ( Article _ _ [] _ _ _:xs) = showPublish xs
showPublish ( Article _ _ n _ _ _:xs) = isAlreadyExists n (showPublish xs)

showPublish ( Thesus _ _ [] _ _ _:xs) = showPublish xs 
showPublish ( Thesus _ _ n _ _ _:xs) = isAlreadyExists n (showPublish xs)

publications = [
    Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001,
    Article "Anatolii" "Covid-19" "Lancet" 2021 12 250,
    Thesus "Artem" "Metaphysics" "Conference of physics" "Amsterdam" 2020 12,
    Book "Andrii" "UI/UX design" "Rivne" "Rivne book" 2020,
    Book "Anton" "Study easy" "Kyiv" "Malyatko" 2021,
    Article "Arsentii" "Web developing" "Webdev" 2021 30 25,
    Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001,
    Book "Sergey" "Elder Rings" "Kherson" "Malyatko" 2001
            ]

--масив значень для тестування створеної функції


isAlreadyExists n l
  | n `elem` l = l
  | otherwise = n:l

