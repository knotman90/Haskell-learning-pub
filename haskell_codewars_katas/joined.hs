module JoinedWords where

joinS :: [String] -> String -> String
joinS (x:xs) s
	| xs == [] = x
	| otherwise = concat $ [x] ++ [s] ++ [joinS xs s]