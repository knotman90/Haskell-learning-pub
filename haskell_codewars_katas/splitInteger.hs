{-We need the ability to divide an unknown integer into a given number of even parts — or at least as even as they can be. The sum of the parts should be the original value, but each part should be an integer, and they should be as close as possible.

For example, given the integer 20, divided into 5 parts, we need [4,4,4,4,4], but divided into 6 parts, we want it to be [4,4,3,3,3,3].

Complete the function so that it returns an array of integer representing the parts. Ignoring the order of the parts, there is only one valid solution for each input to your function!

(Also, there is no reason to test for edge cases: the input to your function will always be valid for this kata.)-}

splitInteger :: Int -> Int -> [Int]
splitInteger a 1 = [a]
splitInteger a b 
	|(a==b)		 = (replicate b 1)
	|(a `mod` b)==0  = (replicate b (adb))
	|otherwise 	 = [adb] ++ (splitInteger (a-adb) (b-1))
	where adb =  (a `div` b)
