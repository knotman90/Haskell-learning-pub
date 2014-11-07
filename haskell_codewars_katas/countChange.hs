countChange' :: Integer -> [Integer] -> Integer
countChange' 0 _  = 1
countChange' _ [] = 0
countChange' n xs@(y:ys) 
	| (n < 0) =0
	| (n == 0) = 1
	| (n > 0) = sum [countChange' (n-h*i) ys | i<-[0..(n `div` h)]]
	where h = head xs 
