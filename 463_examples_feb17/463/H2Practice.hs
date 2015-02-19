module H2Practice where


{-

-- goal: implement each of these functions.

range :: Int -> Int -> Int -> [Int]
addMaybes :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
safeDiv :: Int -> Int -> Maybe Int
fibs :: Int -> [Int]
fibbies :: Int -> [Int]
prefer :: [Maybe Int] -> Maybe Int
search :: String -> [(String,Int)] -> Maybe Int
substring :: String -> String -> Int
substr :: String -> String -> Maybe Int
-}



r2 a b c = [a,(a+c)..b]
    
range :: Int -> Int -> Int -> [Int]
--       start  stop   step
range start stop 0 = error $ "need non-zero step!"
range start stop step | (step>0) && (start>=stop) = []
                      | (step<0) && (start<=stop) = []
                      | otherwise     = start : (range (start+step) stop step)


addMaybes :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
addMaybes (Just x) (Just y) = Just $ x + y
addMaybes _        _        = Nothing

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
-- also okay!
-- safeDiv x y | y==0 = Nothing
safeDiv x y = Just (div x y)


fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))
              
fibs :: Int -> [Int]
fibs 0 = [0]
fibs n = (fib n) : (fibs (n-1))


fibbies :: Int -> [Int]
fibbies n = fibbiesHelper 0 n

fibbiesHelper :: Int -> Int -> [Int]
fibbiesHelper i n | i==n = [fib n]
fibbiesHelper i n = (fib i) : (fibbiesHelper (i+1) n)

prefer :: [Maybe Int] -> Maybe Int
prefer [] = Nothing
prefer ((Just x):xs) = Just x
prefer (Nothing:xs)  = prefer xs


search :: String -> [(String,Int)] -> Maybe Int
search str []     = Nothing
search str ((sval,ival):xs) = if str==sval then Just ival else search str xs

