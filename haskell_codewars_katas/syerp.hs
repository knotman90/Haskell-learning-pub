import Data.List
areYouPlayingBanjo :: String -> String
areYouPlayingBanjo x
  | ((head x) == 'r') || ((head x) =='R') =  x ++ " plays banjo"
  | otherwise = x ++ " does not play banjo"



normalize x
	| x > 9 = sum (intToList' x)
	|otherwise = x


validate :: Integer -> Bool
validate x 
	| even (length list) = ((sum (map normalize (mapK (*2) True list)) `mod` 10)==0)
	| otherwise = (sum (map normalize (mapK (*2) False list)) `mod` 10) ==0
	where list= intToList' x

mapK :: (a->a)-> Bool-> [a]->[a]
mapK _ _ [] = []
mapK _ _ (x:[]) = [x]
mapK f a (x:y:xs) 
	| a = [f x] ++ [y] ++ mapK f a xs
	|otherwise = [x] ++ [f y] ++ mapK f a xs


intToList' :: (Integral a ,Num a) => a ->[a]
intToList' 0 = []
intToList' n =   intToList' (n `quot` 10) ++  [n `mod` 10] -- reverse digits list


seqList :: Int -> Int -> Int -> [Int]
seqList _ _ 0 = []
seqList f c l = [f] ++ seqList (f+c) c (l-1)


countChange :: Integer -> [Integer] -> Integer
countChange 0 x = 0
countChange n y = let b = createLists n y in fromIntegral  (length (nub [ (take (fromInteger b0) (repeat y0 )) ++ (take (fromInteger b1) (repeat y1)) | y0 <-y, y1<- y, b0<-(head b), b1<-(last b) , (y0*b0+y1*b1)==n]))



createLists :: Integer-> [Integer] -> [[Integer]]
createLists _ [] = [[]]
createLists n (x:xs) = [0..(n `div` x)] :  (createLists n xs)


equivLists ::Eq a => [[a]] -> [[a]] ->[[a]]
equivLists [] y = []
equivLists x [] =[]
equivLists (x:xs) l@(y:ys) = [x] ++ equivLists xs (filter (not . equivLst x) l)

equivLst ::Eq a=> [a] -> [a] -> Bool
equivLst [] [] = True
equivLst (x:_) [] = False
equivLst [] (x:_) = False
equivLst (x:xs) l@(y:ys) = (x `elem` l) && (equivLst xs (delete x l) )


fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib' n

fib' :: Int -> [Int] -> [Int]
fib' 1 _= [0]
fib' 2 _= [1]
fib' n x= last(x)+ (last $ init x)





































