module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons RNil x) = [x]
rlistToList (RCons h x) = (rlistToList h) ++ [x]

listToRList :: [a] -> ReverseList a
listToRList lst = foldl foo RNil lst
    where
        foo x y = RCons x y

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor
instance Eq a => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _ = False
    (==) _ RNil = False
    (==) (RCons ha a) (RCons hb b) = (a == b) && (ha == hb)

instance Ord a => Ord (ReverseList a) where
    -- левый Рлист является подстрокой правого 
    --(полная последовательность эллементов левого списка входит в правый)
    -- (даже разоррванная) пример 1, 2, 3 <= 1, 5, 2, 4, 3
    (<=) x y = res x y True
        where 
            res RNil _ t = t
            res (RCons hx x) RNil t = False
            res rlst_x@(RCons hx x) (RCons hy y) t | (x == y) = res hx hy t
            res rlst_x@(RCons hx x) (RCons hy y) t | otherwise = res rlst_x hy t

instance Show a => Show (ReverseList a) where
    show RNil = "[]"
    show rlst = "[" ++ res rlst ++ "]"
        where
            res (RCons RNil a) = show a
            res (RCons head a) = res head ++ ", " ++ show a

instance Semigroup (ReverseList a) where
    (<>) RNil y = y
    (<>) x RNil = x
    --(<>) x (RCons RNil y) = RCons x (RCons RNil y)
    (<>) x (RCons hy y) = RCons ((<>) x hy) y

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend = (<>)

instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons hx x) = RCons (fmap f hx) (f x)
