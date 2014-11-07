import Data.List
import Data.Maybe
-- | Returns the index and the number picked from a board
onlyOneNotEmpty :: [Int] -> Maybe Int
onlyOneNotEmpty l
	|length nz == 1 = Just (head nz)
	|otherwise = Nothing
	where nz =  (findIndices (\x -> x>0) l)

chooseMove :: [Int] -> (Int,Int)
chooseMove l
	|isJust idx =  (fromJust idx,qIdx-2)
	|othwerwise = 			--nothing => piu' di una pila piena
	where 
		idx = onlyOneNotEmpty l	
	 	qIdx = (l !! fromJust idx)
	
