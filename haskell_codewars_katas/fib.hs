
fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib' n [1,0]

fib' :: Int -> [Int] -> Int
fib' 1 x = head x
fib' n l@(x:y:[]) = fib' (n-1) ([x+y]++[x]) 


likes :: [String] -> String
likes [] = "no one likes this"
likes (x:[]) = x ++" likes this"
likes (x:y:[]) = x ++ " and "++y++" like this"
likes (x:y:xs) = x ++ ", "++y++ " and other " ++ (show (length xs)) ++ " like this"



combinations' :: Int -> [a] -> [[a]]
combinations' 0 		_		=[]
combinations' _ 		[] 		= []
combinations' 1 		(x:[])	= [[x]]
combinations' 1 		(x:xs) 	=  [[x]] ++ combinations 1 xs
combinations' k 		l@(x:xs) 
	| (length l) >= (k)  =  (map (x:) (combinations' (k-1) xs)) ++ combinations k xs
	|otherwise = []

combinations :: Int -> [a] -> [[a]]
combinations 0 		_		=[]
combinations _ 		[] 		= []
combinations 1 		(x:[])	= [[x]]
combinations 1 		(x:xs) 	=  [[x]] ++ combinations 1 xs
combinations k 		l@(x:xs) =  combinations' k l ++ combinations k xs



joinS :: [[Char]] -> [Char] -> [Char]
joinS [] 	_  = []
joinS (x:[]) 	_ = x
joinS (x:xs)  (y)  = x++y ++ (joinS xs y)

isMonotone :: Ord a => [a] -> Bool
isMonotone [] = True 
isMonotone (x:[]) = True
isMonotone (x:y:[]) = (x<=y)
isMonotone (x:y:xs) = (x<=y) && isMonotone (y:xs)
 

