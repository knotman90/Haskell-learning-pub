module Last where
import Prelude hiding (last)

last :: [a] -> a
last [x] = x
last (x:xs) = last xs