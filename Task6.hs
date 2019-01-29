module Task6 where

import Todo(todo)

data LinkedTree a = EmptyLT | Fork a (LinkedTree a) (LinkedTree a) (LinkedTree a)

isEmptyLT :: LinkedTree a -> Bool
isEmptyLT EmptyLT = True
isEmptyLT _ = False

tsLT :: LinkedTree a -> LinkedTree a
tsLT EmptyLT = EmptyLT
tsLT ct@(Fork hv pt lt rt) | isEmptyLT pt = ct
tsLT ct@(Fork hv pt lt rt) | otherwise    = tsLT pt

leftestLT :: LinkedTree a -> LinkedTree a
leftestLT EmptyLT = EmptyLT
leftestLT t@(Fork hv pt lt rt) | isEmptyLT lt = t
leftestLT t@(Fork hv pt lt rt) | otherwise    = leftestLT lt

insert_ltLT :: LinkedTree a -> LinkedTree a -> LinkedTree a
insert_ltLT EmptyLT lt_of_rt = lt_of_rt
insert_ltLT lt lt_of_rt@(Fork hv' pt' lt' rt') = res
    where
        res = Fork hv' pt' (spLT lt res) (spLT rt' res)

concLT :: LinkedTree a -> LinkedTree a -> LinkedTree a
concLT lt rt = insert_ltLT lt (leftestLT rt)

find :: (Eq a) => (Ord a) => LinkedTree a -> a -> Bool
find EmptyLT _ = False
find (Fork hv pt lt rt) lfv | lfv == hv = True
find (Fork hv pt lt rt) lfv | lfv > hv = find rt lfv
find (Fork hv pt lt rt) lfv | lfv < hv = find lt lfv

insert :: (Eq a) => (Ord a) => LinkedTree a -> a -> LinkedTree a
insert t iv = res (tsLT t) iv EmptyLT
    where
        res EmptyLT iv lp = (Fork iv lp EmptyLT EmptyLT)
        res ct@(Fork hv pt lt rt) iv lp
            | iv < hv  = lres 
            | iv > hv  = rres
            | iv == hv = ct
            where
                lres = Fork hv lp (res lt iv lp) (spLT rt lres)
                rres = Fork hv lp (spLT lt rres) (res rt iv lp)

remove :: (Eq a) => (Ord a) => LinkedTree a -> a -> LinkedTree a
remove t lfv = res (tsLT t) lfv EmptyLT
    where
        res EmptyLT _ _ = EmptyLT
        res t@(Fork hv pt lt rt) lfv lp
            | hv < lfv  = lres 
            | hv > lfv  = rres
            | hv == lfv = concLT lt rt
            where
                lres = Fork hv lp (res lt lfv lres) (spLT rt lres) 
                rres = Fork hv lp (spLT lt rres) (res rt lfv rres)


spLT :: LinkedTree a -> LinkedTree a -> LinkedTree a
spLT EmptyLT _ = EmptyLT
spLT ct@(Fork hv pt lt rt)  pt_ct = Fork hv pt_ct (spLT lt ct) (spLT rt ct)







-- lp - last parent, hv - have value, pt - parent Tree, lt - left Tree, rt - right Tree
-- t - Tree, ct - current Tree, lfv - looking for value, iv - input value, lfk - loocking for key
-- pt_ct - current tree parent, lres - left res, rres - right res
