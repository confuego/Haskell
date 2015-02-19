module ExprLang1 where

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | N Int
            deriving (Show, Eq)

eval :: Expr -> Int
eval (N n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)
eval (Mul a b) = (eval a) * (eval b)


-- (2+3)*4
e1 = Mul (Add (N 2) (N 3)) (N 4)
-- 2+(3*4)
e2 = Add (N 2) (Mul (N 3) (N 4))
e3 = n2 `Add` (n3 `Mul` n4)

n2 = N 2
n3 = N 3
n4 = N 4

