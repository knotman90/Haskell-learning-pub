module TitleCase (titleCase) where

import Data.Char

titleCase :: String -> String -> String
titleCase "" "" = ""
titleCase minor title = unwords $ [camel (head (words title))] ++ titleCase' (words minor) (tail (words title))

titleCase' :: [String] -> [String] -> [String]
titleCase' c [] = []
titleCase' c (x:xs)
	| (map toLower x) `elem` [map toLower y | y <- c] = [find x c] ++ titleCase' c xs
	| otherwise = [camel x] ++ titleCase' c xs

camel :: String -> String
camel s = map toUpper (take 1 s) ++ [toLower c | c <- tail s]

find :: String -> [String] -> String
find "" _ = ""
find a [] = a
find a (x:xs)
	| (map toLower a) == (map toLower x) = (map toLower x)
	| otherwise = find a xs


-- built-int: words s
split :: String -> [String]
split s = split' 1 s
split' :: Int -> String -> [String] 
split' i s
	| i == (length s) = [s]
	| last (take i s) == ' ' = [take (i-1) s] ++ split' 1 (reverse . take (length s - i). reverse $ s)
	| otherwise = split' (i+1) s