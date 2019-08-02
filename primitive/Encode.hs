module Encode where
import Data.List
import Grid
import DPLL (CNF (..))

-- If a cell (i, j) contains the number n, then ijn, else -ijn
-- http://www.cs.qub.ac.uk/~I.Spence/SuDoku/SuDoku.html

enc :: Grid -> CNF
enc g = prefilled g ++ noMoreThanOne ++ atLeastOne

-- Pre-filled cells
prefilled :: Grid -> CNF 
prefilled g = [[concatNum i j n] | 
               (i, row) <- zip [1..] g,
               (j, n) <- zip [1..] row, 
               n /= 0]

-- Below encoding stays the same for all grids

atLeastOne :: CNF
atLeastOne = [[concatNum i j n | n <- [1..9]] | i <- [1..9], j <- [1..9]] -- Cells
          ++ [[concatNum i j n | j <- [1..9]] | i <- [1..9], n <- [1..9]] -- Rows
          ++ [[concatNum i j n | i <- [1..9]] | j <- [1..9], n <- [1..9]] -- Columns
          ++ [[concatNum i j n | i <- [ii..ii + 2], j <- [jj..jj + 2]] | 
              ii <- [1,4,7], jj <- [1,4,7], n <- [1..9]] -- Blocks

noMoreThanOne :: CNF
noMoreThanOne = [[-x, -y] | row <- atLeastOne, (x:xs) <- tails row, y <- xs]
 
-- e.g. concatNum 1 2 3 = 123
concatNum :: Int -> Int -> Int -> Int
concatNum x y z = x * 100 + y * 10 + z




