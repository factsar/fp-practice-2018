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
insert_ltLT lt lt_of_rt@(Fork hv' pt' lt' rt') = Fork hv' pt' lt rt'

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
            | iv < hv  = (Fork hv pt (res lt iv ct) rt)
        res ct@(Fork hv pt lt rt) iv lp
            | iv > hv  = (Fork hv pt lt (res rt iv ct))
        res ct@(Fork hv pt lt rt) iv lp
            | iv == hv = (Fork hv pt lt rt)

remove :: (Eq a) => (Ord a) => LinkedTree a -> a -> LinkedTree a
remove t lfv = res (tsLT t) lfv
    where
        res EmptyLT _ = EmptyLT
        res t@(Fork hv pt lt rt) lfv | hv < lfv  = res lt lfv
        res t@(Fork hv pt lt rt) lfv | hv > lfv  = res rt lfv
        res t@(Fork hv pt lt rt) lfv | hv == lfv = concLT lt rt

-- lp - last parent, hv - have value, pt - parent Tree, lt - left Tree, rt - right Tree
-- t - Tree, ct - current Tree, lfv - looking for value, iv - input value, lfk - loocking for key
