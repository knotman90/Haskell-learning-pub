import Data.List
import Data.Maybe


sumSbq :: [Int] -> [Int]
sumSbq [] = []
sumSbq (x:[]) = [x]
sumSbq l@(x:xs) = sumSbq (init l) ++ [sum l]   


maxSequence :: [Int] -> [Int]
maxSequence [] = []
maxSequence (x:[]) = [x]
maxSequence l@(x:xs) 
	| (sum here > sum right) = here
	| otherwise = right
	where 
		here = (take (1+(fromJust (findIndex (==(maximum (sumSbq l)))  (sumSbq l)))) l)
		right = (maxSequence xs)


