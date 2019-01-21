module Task2_1 where

import Todo(todo)

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTM | Fork (Integer, v) (TreeMap v) (TreeMap v)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTM

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

lookupTree :: Integer -> TreeMap v -> TreeMap v
lookupTree _ EmptyTM = error "Empty Tree error"
lookupTree lfk t@(Fork (hk, hv) lt rt) 
    | hk == lfk = t
    | hk > lfk = lookupTree lfk lt
    | otherwise = lookupTree lfk rt

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ EmptyTM = error "Empty tree" 
nearestLE lfk t | lfk == keyTM t = prTM t
nearestLE lfk t = res lfk t
    where
        res :: Integer -> TreeMap v -> (Integer, v)
        res lfk EmptyTM = error ""
        res lfk t@(Fork pr lt rt) = 
            if (keyTM t) < lfk  then res' lfk t pr 
                else res lfk lt where 
res' :: Integer -> TreeMap v -> (Integer, v) -> (Integer, v)
res' lfk t@(Fork pr lt rt) cm 
    | (lfk - (fst pr)) < (lfk - (fst cm)) = res' lfk t pr 
    | (((lfk - (fst cm)) < (lfk - (fst (res' lfk lt cm)))) 
        && ((lfk - (fst cm)) < (lfk - (fst (res' lfk rt cm)))) ) = cm
    | (((lfk - (fst cm)) > (lfk - (fst (res' lfk lt cm)))) 
        && ((lfk - (fst (res' lfk lt cm))) < (lfk - (fst (res' lfk rt cm))))) = res' lfk lt cm
    | (((lfk - (fst (res' lfk rt cm))) < (lfk - (fst (res' lfk lt cm)))) 
        && ((lfk - (fst cm)) > (lfk - (fst (res' lfk rt cm))))) = res' lfk rt cm
    | otherwise = error ""

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = EmptyTM
treeFromList (x:xs) = insert x (treeFromList xs)

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTM = []
listFromTree (Fork (k, hv) lt rt) = (listFromTree lt) ++ [(k, hv)] ++ (listFromTree rt)

sortedlistFromTree :: TreeMap v -> [(Integer, v)]
sortedlistFromTree t = res $ listFromTree t
    where
        res l = sortBy (compare `on` fst) l

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ EmptyTM = error "Empty tree"
kMean x t@(Fork pr' lt rt) = res x t where
res x (Fork pr' lt rt)
    | tMPlnght lt == (x - 1) = pr'
    | tMPlnght lt > (x - 1)  = kMean x lt
    | tMPlnght lt < (x - 1)  = kMean (x - (tMPlnght lt) - 1) rt
        where
            tMPlnght EmptyTM = 0
            tMPlnght (Fork _ lt rt) = (tMPlnght lt) + (tMPlnght rt) + 1
