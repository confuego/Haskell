import Prelude hiding (reverse, filter, elem, product, gcd)

collatz::Int -> [Int]
collatz n
	| (n == 1) = [n]
	| ((n `mod` 2) == 0) = n : collatz (quot n 2)
	| ((n `mod` 2) /= 0) = n : collatz ((n*3) + 1)

ack :: Int -> Int -> Int
ack m n
	| (m == 0) = n + 1--ack(m,n+1)
	| (m > 0 && n == 0) = ack (m-1) 1
	| (m > 0 && n > 0) = ack (m-1) (ack m (n-1))

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) =  reverse xs ++ [x]

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
	| f x = x : filter f xs
	| otherwise = filter f xs

elem :: Int -> [Int] -> Bool
elem _ [] = False
elem n (x:xs)
	| (x == n) = True
	| (x /= n) = elem n xs

removeN :: Int -> Int -> [Int] -> [Int]
removeN n v (x:xs)
	| (n >= 1 && x == v) = removeN (n-1) v xs
	| otherwise = (x:xs)


inCommon :: [Int] -> [Int] -> [Int]
inCommon (x:xs) (y:ys) = [ c | c <- (y:ys), c `elem` (x:xs), c `elem` (y:ys)]

isPrime :: Int -> Bool
isPrime p = length [v | v <- [2..(p - 1)], p `mod` v == 0] == 0

primesUnder :: Int -> [Int]
primesUnder p =  [v | v <- [2..(p - 1)], isPrime v == True]

primeFactors :: Int -> [Int]
primeFactors p =  [v | v <- [2..(p - 1)],p `mod` v == 0, isPrime v == True]

coprime :: Int -> Int -> Bool
coprime a b = gcd a b  == 1

gcd :: Int -> Int -> Int
gcd a b = maximum (inCommon (divisors a) (divisors b))

divisors :: Int -> [Int]
divisors n = [v | v <- [1..(n-1)], n `mod` v == 0]

product :: [Int] -> Int
product (x:xs)
	|((xs) == []) = x
	| otherwise = x * product xs

nub :: [Int] -> [Int]
nub [] = [] 
nub [x] = [x]
nub (x1:x2:xs)
	| (x1 == x2) = nub (x1:xs)
	| otherwise = x1 : nub (x2:xs)

data Harvest = Harvest{d::Int,a::Int,p::Int}
			   deriving(Show,Eq)


inspect :: Int -> Harvest
inspect n
	| (sumD > n) = Harvest {d =0,a = sumD, p = 0}
	| (sumD < n) = Harvest {d = sumD,a = 0, p = 0}
	| (sumD == n) = Harvest {d =0,a =0,p = n}
	where sumD = sum (divisors n)

perfectNumbers :: [Int]
perfectNumbers = [v | v <- [1..],sum(divisors v) == v]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x):xs) = x : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs

maxList :: [Int] -> Maybe Int
maxList [] = Nothing
maxList (x1:x2:xs)
	|(length (x1:x2:xs) == 2) = if(x1 > x2) then Just x1 else Just x2
	|(x1 > x2) = maxList (x1:xs)
	|(x2 > x1) = maxList (x2:xs)


