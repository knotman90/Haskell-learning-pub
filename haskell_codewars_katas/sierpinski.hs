module Sierpinsky where

sierpinsky :: Integral a => a -> String
sierpinsky 0 = "L"
sierpinsky x = ""

genrow :: (Integral a) => a -> a -> String
genrow 0 _ = ""
genrow 1 _ = "L"
genrow x y = concat (["L"] ++ take 2 ["    "])