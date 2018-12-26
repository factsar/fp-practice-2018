module Task2_1 where

import Todo(todo)

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTM | Fork (Integer, v) (TreeMap v) (TreeMap v)

min_key_plus :: TreeMap v -> Integer -> Integer
min_key_plus EmptyTM curr_min = curr_min
min_key_plus (Fork pr lt rt) curr_min = 
    min curr_min (min (fst pr) 
        (min (min_key_plus lt (fst pr)) (min_key_plus rt (fst pr)))) 

min_key :: TreeMap v -> Integer
min_key EmptyTM = error "EmptyTM"
min_key t@(Fork pr lt rt) = min_key_plus t (fst pr)

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTM _ = False
contains (Fork (hk , _) lt rt) lfk | hk == lfk = True
                                   | hk > lfk = contains lt lfk
                                   | otherwise = contains rt lfk

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ EmptyTM = error "Empty Tree error"
lookup lfk (Fork (hk, hv) lt rt) 
    | hk == lfk = hv
    | hk > lfk = lookup lfk lt
    | otherwise = lookup lfk rt

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (lfk, v) EmptyTM = Fork (lfk, v) EmptyTM EmptyTM
insert (lfk, v) (Fork (hk, hv) lt rt) | hk == lfk = Fork (hk, v) lt rt
insert (lfk, v) (Fork (hk, hv) lt rt) | hk > lfk  = Fork (hk, hv) (insert (lfk, v) lt) rt
insert (lfk, v) (Fork (hk, hv) lt rt) | hk < lfk  = Fork (hk, hv) lt (insert (lfk, v) rt)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i t = if contains t i then res i t else t
    where
        res :: Integer -> TreeMap v -> TreeMap v
        res lfk (Fork (hk, hv) lt rt) | lfk == hk = res' lt rt
            where
                -- IS THAT CONCATINATION CORRECT ^????
                res' :: TreeMap v -> TreeMap v -> TreeMap v 
                res' EmptyTM rt = rt
                -- res' lt EmptyTM = lt
                res' (Fork v lt' rt') rt = Fork v lt' (res' rt' rt)
        res lfk (Fork (hk, hv) lt rt) | lfk > hk = Fork (hk, hv) (res lfk lt) rt 
        res lfk (Fork (hk, hv) lt rt) | otherwise = Fork (hk, hv) lt (res lfk rt)

prTM :: TreeMap v -> (Integer, v)
prTM t@(Fork pr _ _) = pr
--prTM EmptyTM = error "EmptyTM"

keyTM :: TreeMap v -> Integer
keyTM t = fst $ prTM t 
-- we DNT NEED pattern for the emptyTM 
-- coz we use key_TM in func that knowingly throw away EmptyTM coz of own pattern
-- key_TM EmptyTM = error "EmptyTM"

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ EmptyTM = error "Empty tree" 
nearestLE lfk t | lfk == keyTM t = prTM t
nearestLE lfk t@(Fork (hk, hv) EmptyTM EmptyTM) = (hk, hv)
nearestLE lfk t@(Fork pr lt rt) = res lfk (listFromTree t) pr
    where
        res :: Integer -> [(Integer, v)] -> (Integer, v) -> (Integer, v)
        res lfk [] cpr = cpr
        res lfk lpr@(lh:lt) cpr 
            | (abs $ (fst lh) - lfk) < (abs $ (fst cpr) - lfk)
                = res lfk lt lh
            | otherwise
                = res lfk lt cpr

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = EmptyTM
treeFromList (x:xs) = insert x (treeFromList xs)

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTM = []
--listFromTree (Fork (k, hv) EmptyTM EmptyTM ) = [(k, hv)]
listFromTree (Fork (k, hv) lt rt) = [(k, hv)] ++ (listFromTree lt) ++ (listFromTree rt)

kMean :: Integer -> TreeMap v -> (Integer, v)
kMean kstat t = res kstat t (min_key t) 0
    where 
        res :: Integer -> TreeMap v -> Integer -> Integer -> (Integer, v)
        res kstat t curr_key curr_stat |
            kstat == curr_stat = (curr_key, (lookup curr_key t))
        res kstat t curr_key curr_stat |
            kstat < curr_stat = res kstat t (next_key t curr_key) (curr_stat + 1)
        res kstat t curr_key curr_stat |
            otherwise = error $ "something went wrong with kMean for kstat = " ++ show kstat

        next_key :: TreeMap v -> Integer -> Integer
        next_key t prev = 
            if contains t (prev + 1) then (prev + 1)
            else (next_key t (prev + 1))
