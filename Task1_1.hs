module Task1_1 where

import Todo(todo)

data Operator = Pl_ | Mi_ | Mu_ 
    deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ op :: Operator, lhv :: Term, rhv :: Term } -- бинарная операция
    deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm Pl_ l r
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm Mi_ l r
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm Mu_ l r

infixl 1 |+|
infixl 1 |-|
infixl 2 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
    Variable x | x == varName -> replacement
    BinaryTerm op l r -> 
        BinaryTerm op (replaceVar varName replacement l) (replaceVar varName replacement r)
    _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
    BinaryTerm op l r -> 
        case (op, l_, r_) of
            (Pl_, IntConstant 0,               _)     -> r_
            (Pl_,             _,   IntConstant 0)     -> l_
            (Mu_, IntConstant 1,               _)     -> r_
            (Mu_,             _,   IntConstant 1)     -> l_
            (Mu_, IntConstant 0,               _)     -> IntConstant 0
            (Mu_,             _,   IntConstant 0)     -> IntConstant 0
            (Mi_,             _,   IntConstant 0)     -> l_
            (Pl_, IntConstant l_, IntConstant r_)     -> IntConstant $ l_ + r_
            (Mi_, IntConstant l_, IntConstant r_)     -> IntConstant $ l_ - r_
            (Mu_, IntConstant l_, IntConstant r_)     -> IntConstant $ l_ * r_
            _ -> BinaryTerm op l_ r_
        where
            l_ = evaluate(l)
            r_ = evaluate(r)
    _ -> expression
