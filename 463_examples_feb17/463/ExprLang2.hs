
module ExprLang2 where

-- choice: use infix constructors. visually appealing, nothing more.
data Expr = Expr :+ Expr | Expr :- Expr | Expr :* Expr | N Int
          | B Bool | Iff Expr Expr Expr
            deriving (Show, Eq)


-- choice: use our own newly defined type for the value space; this is
-- better when we have different types of things to return, such as
-- adding booleans and pairs to our language.
data Val = VN Int | VB Bool  deriving (Show, Eq)

eval :: Expr -> Val
eval (N n) = VN n
eval (a :+ b) = case (eval a, eval b) of
                  (VN a', VN b') -> VN $ a' + b'
                  _ -> error $ "bad add."
                                    
eval (a :- b) = case (eval a, eval b) of
                  (VN a', VN b') -> VN $ a' - b'
eval (a :* b) = case (eval a, eval b) of
                  (VN a', VN b') -> VN $ a' * b'

eval (B True ) = VB True
eval (B False) = VB False

eval (Iff a b c) = case (eval a) of
                     VB True -> eval b
                     VB False-> eval c
                     _ -> error "non-boolean guard!"
                 
-- choice: 
n2 = N 2
n3 = N 3
n4 = N 4

-- (2+3)*4
e1 = (n2 :+ n3) :* n4
-- 2+(3*4)
e2 = n2 :+ (n3 :* n4)

result = eval e1
