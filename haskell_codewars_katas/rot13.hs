module Rot13 where

import Data.Char

rot13 :: String -> String
rot13 "" = ""
rot13 (x:xs) = [shift13 x] ++ rot13 xs

shift13 :: Char -> Char
shift13 x
	| x `elem` ['a'..'z'] = ['a'..'z'] !! ((pick13 x ['a'..'z']) `mod` 26)
	| x `elem` ['A'..'Z'] = ['A'..'Z'] !! ((pick13 x ['A'..'Z']) `mod` 26)
	| otherwise = x

pick13 :: Char -> [Char] -> Int
pick13 c s = (indexof c s) + 13

--prova :: Char -> [Char] -> Bool
--prova c [] = False
--prova c s
--	| c == head s = True
--	| otherwise = prova c (tail s)

indexof :: Char -> [Char] -> Int
indexof c (s:xs)
	| c == s = 0
	| otherwise = index' c xs 1

index' :: Char -> [Char] -> Int -> Int
index' c (s:xs) i
	| c == s = i
	| otherwise = index' c xs i+1