module Validate where

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sum . zipWith ($) (cycle [id, sum . digits . (*2)]) . reverse . digits
  where digits = map (read . return) . show

validatex :: Integer -> Bool
validatex 0 = True
validatex x = ((sum $ nine $ evenodd $ intolist x) `mod` 10) == 0

intolist :: Integer -> [Integer]
intolist 0 = []
intolist x = intolist (x `div` 10) ++ [x `mod` 10]

evenodd :: (Num a) => [a] -> [a]
evenodd [] = []
evenodd x
	| ((length x) `mod` 2 == 0 ) = multEven x
	| otherwise = multOdd x

multEven :: (Num a) => [a] -> [a]
multEven [] = []
multEven (x:[]) = [x * 2]
multEven (x:y:xs) = [x * 2] ++ [y] ++ multEven xs 

multOdd :: (Num a) => [a] -> [a]
multOdd [] = []
multOdd(x:[]) = [x]
multOdd (x:y:xs) = [x] ++ [y * 2] ++ multOdd xs

nine :: [Integer] -> [Integer]
nine [] = []
nine (x:xs)
	| (x > 9) = [sum (intolist x)] ++ nine xs
	| otherwise = [x] ++ nine xs
