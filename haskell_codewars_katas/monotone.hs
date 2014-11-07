module Monotone where

-- | Return true if the elements of the list are non-decreasing.
--   If the list is empty, return True.
isMonotone :: Ord a => [a] -> Bool
isMonotone [] = True
isMonotone (x:[]) = True
isMonotone (x:y:[]) = x <= y
isMonotone (x:y:xs) = x <= y && isMonotone (y:xs) 
