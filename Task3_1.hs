module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

simplify :: WeirdPeanoNumber -> WeirdPeanoNumber
simplify Zero = Zero
simplify x = res x 0 0
    where
        res :: WeirdPeanoNumber -> Integer -> Integer -> WeirdPeanoNumber
        res (Succ l) m n = res l (m + 1) n
        res (Pred l) m n = res l m (n + 1)
        res Zero m n | (m - n) == 0 = Zero
        res Zero m n | otherwise = res' (m - n) Zero
            where
                res' :: Integer -> WeirdPeanoNumber -> WeirdPeanoNumber
                res' 0 y = y
                res' x y | x > 0 = res' (x - 1) (Succ y)
                res' x y | x < 0 = res' (x + 1) (Pred y)

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show Zero = "Zero"
    show (Succ x) = "Succ $ " ++ show x
    show (Pred x) = "Pred $ " ++ show x

instance Num WeirdPeanoNumber where
    (+) x Zero = x
    (+) Zero y = y
    (+) (Pred (Succ nn)) y = (+) nn y
    (+) (Succ (Pred nn)) y = (+) nn y
    (+) x (Pred (Succ mm)) = (+) x mm
    (+) x (Succ (Pred mm)) = (+) x mm
    (+) (Succ n) (Pred m)  = (+) n m
    (+) (Succ n) y         = (+) n (Succ y)
    (+) (Pred n) (Succ m)  = (+) n m
    (+) (Pred n) y         = (+) n (Pred y)

    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) (Succ Zero) y = y
    (*) x (Succ Zero) = x
    (*) x y = res x y 0
        where
            res :: WeirdPeanoNumber -> WeirdPeanoNumber -> Integer -> WeirdPeanoNumber
            res a b c | c == 0 = res a b (toInteger b)
            res a b c | otherwise = res' a c 
                where
                    res' :: WeirdPeanoNumber -> Integer -> WeirdPeanoNumber
                    res' n 0 = n
                    res' n m | m `mod` 2 == 0 = res' (n + n) (m `div` 2) 
                    res' n m | otherwise = res' (Succ n) (m - 1)


    abs x | x < Zero = negate x
    abs x | otherwise = x

    signum x | x == Zero = Zero
    signum x | x > Zero = Succ Zero
    signum x | x < Zero = Pred Zero

    negate Zero = Zero
    negate (Succ x) = Pred (negate x)
    negate (Pred x) = Succ (negate x)

    fromInteger x | x == 0 = Zero
    fromInteger x | x < 0 = Pred (fromInteger (x + 1))
    fromInteger x | x > 0 = Succ (fromInteger (x - 1))

instance Eq WeirdPeanoNumber where
    (==) x y = (==) $ (simplify x) (simplify y)
    (==) Zero Zero = True
    (==) (Succ x) (Succ y) = (==) x y
    (==) (Pred x) (Pred y) = (==) x y
    (==) _ _ = False

instance Ord WeirdPeanoNumber where
    (<=) x y | toInteger x <= toInteger y = True
    (<=) x y | otherwise = False

instance Real WeirdPeanoNumber where
    toRational x = toRational (toInteger x)

instance Enum WeirdPeanoNumber where
    toEnum x = fromInteger (toInteger x)
    fromEnum x = fromIntegral (toInteger x)

instance Integral WeirdPeanoNumber where
    toInteger x = res x 0
        where
            res :: WeirdPeanoNumber -> Integer -> Integer
            res Zero c = c
            res (Succ a) c = res a (c + 1)
            res (Pred a) c = res a (c - 1)

    quotRem a b  = (fromInteger div, fromInteger mod)
        where
            (div, mod) = quotRem (toInteger a) (toInteger b)
