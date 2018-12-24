module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x [] = x
foldl f x (lh:lt) = foldl f (f x lh) lt

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x
foldr f x (lh:lt) = f lh (foldr f x lt)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x = case (f x) of
    Nothing -> []
    Just (a, b) -> a : (unfoldr f b)

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldr (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (lh:lt) = foldr (\lh lt -> (f lh):lt) [] lt

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product l = foldr (*) 1 l 

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes l = foldl f [] l
    where
        f l' (Just x) = x:l'
        f l' Nothing  = l'

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal l = fst $ foldl f ([], 0) l
    where
        f (r, n) l = (r ++ [l!!n], n+1)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f l = foldr (\x s -> if f x then s else x:s) [] l

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem lfe l = foldr (\x s -> (||) s (x == lfe)) False l

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from
    where
        f x | x < to    = Just (x, x+step)
        f x | otherwise = Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append ll lr = foldr (\x lt -> x:lt) ll lr

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups l n = res l n [] 0 []
    where
        res :: [a] -> Integer -> [a] -> Integer -> [[a]] -> [[a]]
        res []      n rl c rll          = rl:rll
        res (lh:lt) n rl c rll | c == 0 = res lt n [lh]    1                 rll 
        res (lh:lt) n rl c rll | c == n = res lt n []      0       ((lh:rl):rll)
        res (lh:lt) n rl c rll | c < n  = res lt n (lh:rl) (c+1)             rll
