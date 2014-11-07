module DigitalRoot where

digitalRoot :: Integral a => a -> a
digitalRoot x
	| x < 10 = x
	| otherwise = digitalRoot (modz x)

modz :: Integral a => a -> a
modz 0 = 0
modz x = x `mod` 10 + modz (x `div` 10)