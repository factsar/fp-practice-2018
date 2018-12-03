module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd 0 0 = error "Unsupported: both numbers are equal to 0."
gcd x y = if ((x < 0) || (y < 0)) 
    then error "Unsupported: negative arguments."
    else res_ x y
    where res_ a 0 = a
          res_ a b =  res_ b (a `rem` b)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow 0 0 = error "undefined" 
pow 0 _ = 1
pow x 0 = 1
pow x y | y < 0 = error "Unsupported: negative power value."
pow x y = res_ x y 1
      where
         res_ x y r | y == 0 = r
         res_ x y r | y `mod` 2 == 0 = res_ (x * x) ((y `div` 2)-1) r*x*x
         res_ x y r | otherwise = res_ x (y-1) (r * x)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | x <= 1 = False
isPrime x | x == 2 = True
isPrime x | x `mod` 2 == 0 = False
isPrime x | otherwise = if (floor (sqrt (fromIntegral x))) `mod` 2 == 0 then res' ((floor (sqrt (fromIntegral x))) - 1) else  res' (floor (sqrt (fromIntegral x)))
    where
        res' i | i == 1 = True
        res' i | (Task1_2.gcd x i) > 1 = False
        res' i = res' (i - 2)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points | points == [] = 0
shapeArea points | otherwise = reski points
    where
        reski liist@(xh:xt) = 0.5 * abs (resu liist xt 0 xh)
             where
                 resu lsti@(lh:lt) [] c d = c + (fst lh * snd d - snd lh * fst d)
                 resu lsti@(lh:lt) lsti_@(lh_:lt_) c d = resu lt lt_ (c + (fst lh * snd lh_ - snd lh * fst lh_)) d

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
