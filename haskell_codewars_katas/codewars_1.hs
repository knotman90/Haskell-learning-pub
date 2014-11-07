module Multiply.Bug.Fix where

multiply :: Int -> Int -> Int
multiply a b = a * b

module Person where

data Person = Person { name :: String }

greet :: Person -> String -> String
greet person otherName = 
  "Hi " ++ otherName ++ ", my name is " ++ name