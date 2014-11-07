module Change where

countChange :: Integer -> [Integer] -> Integer
countChange n (x:[])
	| n == x = 1
	| otherwise = 0
countChange n (x:y:[])
	| x+y == n = 1
	| x+y > n = 0
	| otherwise = 0

valid :: [Integer] -> Integer -> Bool
valid l x
	| (sum l) == x = True
	| otherwise = False

