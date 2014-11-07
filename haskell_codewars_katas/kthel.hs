module Kth where
import Prelude hiding ((!!))

elementAt :: [a] -> Int -> a
elementAt l s = last (take s l)