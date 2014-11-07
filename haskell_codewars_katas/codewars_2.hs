module Person where

data Person = Person { name :: String }

greet :: Person -> String
greet p = name p