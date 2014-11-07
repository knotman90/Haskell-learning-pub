module Zeros where

zeros :: Int -> Int
zeros x  = trailing (0 (fact 12))

fact :: (Integral a) => a -> a
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

trailing :: (Integral a) => a -> a -> a
trailing i x
	| (x `mod` 10) == 0 = trailing ((i+1) (x `div` 10))
	| otherwise = 0