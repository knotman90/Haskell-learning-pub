import Data.Char
import Data.List
-- length ([ (m,i,j,psy m i j) | m<-[0..(mg-1)], i<-[0..(2^((mg)-m)-1)], j<-[0..(2^(mg)-1)]])

ps x 
	| (x>=0 && x<0.5) = 1
	| (x>=0.5 && x<1) = -1
	| otherwise =0

psy m i j = let mq = (2^m) in 1/(sqrt (mq)) * (ps ((j-(mq)*i)/(mq)))



sortme :: [String] -> [String]
sortme x = sortBy compareStr x;

compareStr :: String -> String -> Ordering
compareStr [] (x:_) = LT
compareStr (x:_) [] = GT
compareStr [] [] = EQ
compareStr  (x:xs) (y:ys) 
	| (toLower x)>(toLower y) = GT
	| (toLower x)<(toLower y) = LT
	|otherwise = compareStr xs ys








-- | Returns Just the k-th element of the list, or Nothing if k is out of bounds.
elementAt :: Int -> [a] -> Maybe a
elementAt n xs
  | (null val)  = Nothing
  | otherwise = Just (head val)
	where val = drop n xs



balancedParens' :: Int -> [String]
balancedParens' 0 = [""]
balancedParens' 1 = ["()"]
balancedParens' x = nub ((map (\x -> "("++x++")" ) balPrec) ++ (map ("()"++) balPrec) ++ (map (++"()") balPrec))
	where balPrec =  balancedParens' (x-1)


bal :: Int -> Int -> [String] -> [String]
bal 0 1 l = map (++")") l
bal 0 c l = bal 0 (c-1) (map (++")") l)
bal _ 0 l = map (++")") l
bal a c l 
	| (a==c) = (bal (a-1) c (map (++"(") l ))
	| (a<c) =  (bal (a-1) c (map (++"(") l )) ++ (bal a (c-1) (map (++")") l))

balancedParens :: Int -> [String]
balancedParens 0 = [""]
balancedParens k =  bal k k ""






