module Encode where
import Data.List
import Grid
import DPLL (Literal (..), CNF (..), neg)

-- If a cell (i, j) contains the number n, then ijn, else -ijn
-- http://www.cs.qub.ac.uk/~I.Spence/SuDoku/SuDoku.html

enc :: Grid -> CNF (Int, Int, Int)
enc g = prefilled g ++ noMoreThanOne ++ atLeastOne

-- Pre-filled cells
prefilled :: Grid -> CNF (Int, Int, Int)
prefilled g = [[P (i, j, n)] | 
               (i, row) <- zip [1..] g,
               (j, n) <- zip [1..] row, 
               n /= 0]

-- Below encoding stays the same for all grids

atLeastOne = [[P (i, j, n) | n <- [1..9]] | i <- [1..9], j <- [1..9]] -- Cells
          ++ [[P (i, j, n) | j <- [1..9]] | i <- [1..9], n <- [1..9]] -- Rows
          ++ [[P (i, j, n) | i <- [1..9]] | j <- [1..9], n <- [1..9]] -- Columns
          ++ [[P (i, j, n) | i <- [ii..ii + 2], j <- [jj..jj + 2]] | 
              ii <- [1,4,7], jj <- [1,4,7], n <- [1..9]] -- Blocks

noMoreThanOne = [[neg x, neg y] | row <- atLeastOne, (x:xs) <- tails row, y <- xs]
 
