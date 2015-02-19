module DenExpr where

import Data.Char (ord)

data DecNum = Digits String                                deriving (Show, Eq)
data Op = Plus | Minus | Mul                               deriving (Show, Eq)
data Expr = Num DecNum | Var String | BinOp Expr Op Expr   deriving (Show, Eq)

data Stmt = Set String Expr | Seq Stmt Stmt                deriving (Show, Eq)

type State = [(String,Int)]



-- Data.Char.ord gives us the ASCII code. '0' is 48.

-- evalD: evaluate Digits.
evalD :: DecNum -> Int
evalD (Digits [])   = error "empty number!"
evalD (Digits [c])  = (ord c) - 48
evalD (Digits (cs)) = 10*(evalD (Digits (init cs))) + ((ord (last cs))-48)

-- evalE: evaluate Expressions.
evalE :: Expr -> [(String,Int)] -> Int
evalE (Var var)state = varmap var state
evalE (Num dn) state = evalD dn
evalE (BinOp e1 Plus  e2) state = (evalE e1 state) + (evalE e2 state)
evalE (BinOp e1 Minus e2) state = (evalE e1 state) - (evalE e2 state)
evalE (BinOp e1 Mul   e2) state = (evalE e1 state) * (evalE e2 state)

varmap :: String -> [(String,v)] -> v
varmap var ((s,v):rest) = if s==var then v else varmap var rest
varmap var [] = error ("couldn't find "++var++".")

-- evalS: evaluate Statements (just assignments and sequences).
evalS :: Stmt -> State -> State
evalS (Set var e) state = (var, evalE e state):state
evalS (Seq s1 s2) state = evalS s2 (evalS s1 state)
                          
-- some simple expressions that we can test with.
d1 = Digits "1"
d6 = Digits ['6']
d7 = Digits ['7']
d23 = Digits ['2','3']		-- same as writing d23 = Digits "23"

va = Var "a"
vb = Var "b"


st :: [(String,Int)]
st = [("a",10),("b",2),("c",30)]

t1 = BinOp (Num d6) Plus (Num d7)
t2 = BinOp (Num d23) Minus (Num d7)
t3 = Num d6
t4 = Var "c"
t5 = BinOp (Num d7) Plus va


sInc :: String -> Stmt
sInc v = Set v (BinOp (Var v) Plus (Num d1))

s1 = Set "a" (Num (Digits ['1','2','3']))
s2 = Seq s1 (sInc "a")

sinca = sInc "a"


{-
-- run tests (over in ghci) like these:

evalD d23

evalE t1 st
evalE t2 st
-- and the rest...


st  -- take a look at it, see what changes after the below commands:

evalS s1 st
evalS s2 st
evalS (Seq sinca (Seq sinca sinca)) st

-}

