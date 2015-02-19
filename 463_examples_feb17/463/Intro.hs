module Intro where

data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet    deriving (Show,Eq)



minVal :: [Int] -> Int
minVal [] = 0 
minVal xs = minValHelper xs (head xs)


minValHelper :: [Int] -> Int -> Int
minValHelper []     mv = mv
minValHelper (x:xs) mv = if x<mv
	     	       	 then minValHelper xs x
			 else minValHelper xs mv