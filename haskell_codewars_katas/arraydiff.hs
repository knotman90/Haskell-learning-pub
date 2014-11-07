module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference [] _ = []
difference x [] = x
difference l@(x:xs) (y:ys) =  difference (remove' y l) ys

remove' :: Eq a => a -> [a] -> [a]
remove' _ [] = []
remove' x (y:ys)
	| x == y = remove' x ys 
	| otherwise = [y] ++ remove' x ys