
--Завдання1
a = [(1.5,('c',True),5)]


--Завдання2
--а) як один кортеж
crossingCircles :: ((Float,Float),Float,(Float,Float),Float) -> Bool

crossingCircles ((x1,y1),r1,(x2,y2),r2) | (d > (r1+r2)) || ((d <= 1.0E-8) && (abs (r1-r2) > 1.0E-8)) = False
                                                         | otherwise = True 
                                                         where d = sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))
--б) без використання кортежів чи списків                                                    
crossingCircles2 :: Float -> Float -> Float -> Float -> Float -> Float -> Bool

crossingCircles2 x1 y1  r1 x2 y2 r2 | (d > (r1+r2)) || ((d <= 1.0E-8) && (abs (r1-r2) > 1.0E-8)) = False
                                                         | otherwise = True 
                                                         where d = sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))