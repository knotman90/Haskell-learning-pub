module MinimumDot where
import Data.List
minDot :: (Ord a, Num a) => [a] -> [a] -> a
minDot xs ys = sum $ zipWith (\a b -> a*b) (sort xs) ((reverse . sort) ys)
