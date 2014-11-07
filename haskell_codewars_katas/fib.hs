module Fib where

fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = round $ phi ** fib (n-1) / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2