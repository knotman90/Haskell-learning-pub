module LengthLexicographic where

newtype LengthList a = LengthList [a]
 deriving(Show,Eq)
 
instance Ord a => Ord (LengthList a) where
  compare (x:xs) (y:ys) = LT



  {-|compare x@(a:as) y@(b:bs)
    | length x > length y = GT
    | length x < length y = LT
    | otherwise =
    	case compare a b of
    		GT -> GT
    		LT -> LT
     		EQ -> compare as bs|-}
