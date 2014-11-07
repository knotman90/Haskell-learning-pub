module Digitize where

digitize :: Integer -> [Integer]
digitize x
	| x `div` 10 == 0 = [x]
	| otherwise = digitize (x `div` 10) ++ [x `mod` 10]  