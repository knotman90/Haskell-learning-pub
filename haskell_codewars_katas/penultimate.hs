module Penultimate where

penultimate :: [a] -> a
penultimate x = x !! (length (tail x) - 1)