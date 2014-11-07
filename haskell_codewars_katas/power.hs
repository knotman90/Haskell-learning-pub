module Power where

power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = (map ([x] ++) (power xs)) ++ power xs