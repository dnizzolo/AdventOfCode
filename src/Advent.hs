module Advent (combinationsOf2) where

import Data.List (tails)

combinationsOf2 :: [a] -> [(a, a)]
combinationsOf2 l = [(x, y) | (x : ys) <- tails l, y <- ys]
