module Palindromic where

palindromize :: Integer -> (Int, Integer)
palindromize x
	| (digitize x) == (reverse $ digitize x) = (0, x)
	| otherwise = iterate' 0 x

digitize :: Integer -> [Integer]
digitize x
	| x `div` 10 == 0 = [x]
	| otherwise = digitize (x `div` 10) ++ [x `mod` 10]

base10 :: [Integer] -> Integer
base10 (x:[]) = x * (10 ^ 0)
base10 (x:xs) = x * (10 ^ (length xs)) + base10 xs

iterate' :: Int -> Integer -> (Int, Integer)
iterate' x y
	| (digitize y) == (reverse $ digitize y) = (x, y)
	| otherwise = iterate' (x+1) (y + base10 (reverse $ digitize y))