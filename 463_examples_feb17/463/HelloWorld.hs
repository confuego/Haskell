module HelloWorld where

import Data.Char

main = putStr "Hello, World!\n"


add x y = x + y

inc x = x + 1

fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs :: [Int]
fibs = 1 : 1 : (zipWith (+) fibs (drop 1 fibs))


data Boole = Tru | Fls                      deriving (Show, Eq)
data StopLight = Red | Green | Amber        deriving (Show, Eq)
data Simpson = Homer | Marge | Lisa | Bart
             | Maggie | Santa'sLittleHelper deriving (Show, Eq)

data Eether = This Bool | That Int Int Int  deriving (Show, Eq)

-- type String = [Char]
type State = [(String, Value)]

useEether (This b)     = if b then 100 else 0
useEether (That x y z) = x+y+z

go :: Eether -> Int
go x = case x of
         This b     -> if b then 100 else 5
         That a b c -> a + b + c


this = This True
v1 = That 2 4 6
v2 = That 7 8 9
v3 = That 10 10


first [] = error "empty!"
first (x:xs) = x













