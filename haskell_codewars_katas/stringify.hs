module Stringify where

numberToString :: Int -> String
numberToString num = concat $ map (show) (digitize num)

digitize :: Int -> [Int]
digitize x
	| x `div` 10 == 0 = [x]
	| otherwise = digitize (x `div` 10) ++ [x `mod` 10]  