

-- Untyped Lambda Calculus basics

module ULC where

import Prelude hiding(id)

-- representation of terms in the ULC.
data Tm = Var String | Lam String Tm | App Tm Tm
        | Tru | Fls | If Tm Tm Tm
        | Zero | Succ Tm | Pred Tm
        | Pair Tm Tm | Fst Tm | Snd Tm

          deriving (Eq)  -- there's a better Show instance below that you could use...


-- rather than use a separate datatype for values, we just define a predicate.
is_val (Lam _ _) = True
is_val Tru       = True
is_val Fls       = True
is_val Zero      = True
is_val (Succ  t) = all_succs t
is_val (Pred  t) = all_preds t
is_val (Pair t1 t2) = (is_val t1) && (is_val t2)
is_val _         = False

all_succs (Succ t) = all_succs t
all_succs (Zero)   = True
all_succs _        = False

all_preds (Succ t) = all_preds t
all_preds (Zero)   = True
all_preds _        = False


                     

-- I want it to *look* like the lambda calculus, too! remove Show from
-- Tm's deriving() clause and add this one.

instance Show Tm where
  -- ignore these first two for now.
  show t | alpha_equiv t true = "true"
  show t | alpha_equiv t false = "false"
  -- the basic show definition (better than the derived version)
  show (Var s) = s
  show (Lam s t) = "(\\"++s++"."++show t++")"
  show (App t1 t2) = "("++show t1++" "++show t2++")"
  show Tru = "true"
  show Fls = "false"
  show (If a b c) = "if "++show a++" then "++show b++" else "++show c
  show (Zero) = "0"
  show (Succ v) = "succ "++show v
  show (Pred v) = "pred "++show v
  show (Pair a b) = "("++show a++","++show b++")"
  show (Fst a) = "fst "++show a
  show (Snd a) = "snd "++show a


-- -----------------------------------------------------------------------------

-- evaluation takes us from a term to a value.
-- eval :: Tm -> V  (except that a value is just a subset of our terms!)


eval :: Tm -> Tm
eval t | is_val t = t
eval (Var s) = error $ "found a free variable, whoops! ("++s++")"
{- E-App1-}
eval (App t1 t2) | not (is_val t1) = eval (App (eval t1) t2)
{- E-App2-}                                     
--                 | not (is_val t2) = eval (App t1 (eval t2))
{- E-AppLambda -}
eval a@(App (Lam s t) arg) =  eval (subst s arg t)
{-E-IfTrue-}
eval (If Tru b c) = eval b
{-E-IfFalse-}
eval (If Fls b c) = eval c
{- E-If -}
eval (If a b c) = eval (If (eval a) b c)

eval (Succ (Pred t)) = eval t
eval (Pred (Succ t)) = eval t
eval (Succ t) | not (is_val t) = eval (Succ (eval t))
eval (Pred t) | not (is_val t) = eval (Pred (eval t))

eval (Pair x y) | not(is_val x) = eval ( Pair (eval x) y         )
                | otherwise     = eval ( Pair  x        (eval y) )

eval (Fst (Pair x y)) = eval x
eval (Fst t) = eval (Fst(eval t))

eval (Snd (Pair x y)) = eval y
eval (Snd t) = eval (Snd(eval t))



eval others = error $ "couldn't evaluate: "++show others


-- substitution helps us evaluate an application of a lambda with a value.
subst v rep (Var s) | v==s      = rep
                    | otherwise = (Var s)

subst v rep (Lam x t) | v==x      = Lam x t
                      | otherwise = Lam x (subst v rep t)

subst v rep (App t1 t2) = App (subst v rep t1) (subst v rep t2)
subst v rep (If a b c) = If (subst v rep a) (subst v rep b) (subst v rep c)
subst v rep Tru = Tru
subst v rep Fls = Fls
subst v rep Zero = Zero
subst v rep (Pred s) = Pred (subst v rep s)
subst v rep (Succ s) = Succ (subst v rep s)

-- -----------------------------------------------------------------------------

-- some example definitions

-- the "identity" function
id = Lam "s" (Var "s")

true  = Lam "x" ( Lam "y" (Var "x"))
false = Lam "x" $ Lam "y" (Var "y")



-- funny names won't clash with built-in definitions.
nott = Lam "m" $ (Var "m") `App` false `App` true

andd = Lam "m" $ Lam "n" $ (Var "m") `App` (Var "n") `App` (Var "m")
orr  = Lam "m" $ Lam "n" $ (Var "m") `App` (Var "m") `App` (Var "n")

iff  = Lam "b" $ Lam "m" $ Lam "n" $ (Var "b") `App` (Var "m") `App` (Var"n")

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- some examples (eval them)

-- both (boolean) inputs for not.
not_t = App nott true
not_f = App nott false

-- all (boolean) inputs for and.
t_and_t = andd `App` true  `App` true  
t_and_f = andd `App` true  `App` false 
f_and_t = andd `App` false `App` true  
f_and_f = andd `App` false `App` false 

-- all (boolean) inputs for or.
t_or_t = (App (App orr true) true ) 
t_or_f = orr `App` true  `App` false 
f_or_t = orr `App` false `App` true  
f_or_f = orr `App` false `App` false 

-- all (boolean) versions of if.
if_f_f_f = iff `App` false `App` false `App` false
if_f_f_t = iff `App` false `App` false `App` true
if_f_t_f = iff `App` false `App` true  `App` false
if_f_t_t = iff `App` false `App` true  `App` true
if_t_f_f = iff `App` true  `App` false `App` false
if_t_f_t = iff `App` true  `App` false `App` true
if_t_t_f = iff `App` true  `App` true  `App` false
if_t_t_t = iff `App` true  `App` true  `App` true


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- how do we actually get those "true" and "false" strings to show up?
-- using the notion of alpha equivalence: other than choice of names, are two
-- terms identical?


-- for convenience, let's be able to change the variable used in a lambda.
rename sold snew (Lam s t) = Lam (if s==sold then snew else s) (rename sold snew t)
rename sold snew (Var s) = if s==sold then Var snew else Var s
rename sold snew (App t1 t2) = App (rename sold snew t1) (rename sold snew t2)


-- if we re-name lambda variables throughout to match, are these two
-- terms identical?

alpha_equiv (Var a) (Var b) = a==b

alpha_equiv lam1@(Lam s1 t1) lam2@(Lam s2 t2) =
    if s1==s2
    then alpha_equiv t1 t2 
    else alpha_equiv lam1 (rename s2 s1 lam2)

alpha_equiv (App t11 t12) (App t21 t22) =
    alpha_equiv t11 t12 && alpha_equiv t21 t22

alpha_equiv a b = False

-- I am a lazy typer sometimes.
ae = alpha_equiv

{-
-- now we can use alpha_equiv during our Show instance. (Same as above)

instance Show Tm where
  show t | alpha_equiv t true = "true"
  show t | alpha_equiv t false = "false"
  show (Var s) = s
  show (Lam s t) = "\\"++s++"."++show t
  show (App t1 t2) = "("++show t1++" "++show t2++")"

-}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


