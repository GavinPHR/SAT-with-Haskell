import DPLL
import Grid
import Data.List (sort, tails)

-- For compilation, uncomment the line below
-- main = solve exampleGrid

solve' :: Grid -> Valuation (Int, Int, Int)
solve' g = sort [n | n <- dpll (encode g) [], isPos n]
           where
            isPos (P _) = True
            isPos (N _) = False

format :: Valuation (Int, Int, Int) -> Grid
format [] = []
format v  = let
                (l, r) = splitAt 9 v
            in
                map (getNum) l : format r
            where
                getNum (P (_,_,x)) = x

solve :: Grid -> IO ()
solve g = mapM_ print $ format $ solve' g

-- If a cell (i, j) contains the number n, then ijn, else -ijn
-- http://www.cs.qub.ac.uk/~I.Spence/SuDoku/SuDoku.html

encode :: Grid -> CNF (Int, Int, Int)
encode g = prefilled g ++ noMoreThanOne ++ atLeastOne

-- Pre-filled cells
prefilled :: Grid -> CNF (Int, Int, Int)
prefilled g = [[P (i, j, n)] | 
               (i, row) <- zip [1..] g,
               (j, n) <- zip [1..] row, 
               n /= 0]

-- Below encoding stays the same for all grids
atLeastOne :: CNF (Int, Int, Int)
atLeastOne = [[P (i, j, n) | n <- [1..9]] | i <- [1..9], j <- [1..9]] -- Cells
          ++ [[P (i, j, n) | j <- [1..9]] | i <- [1..9], n <- [1..9]] -- Rows
          ++ [[P (i, j, n) | i <- [1..9]] | j <- [1..9], n <- [1..9]] -- Columns
          ++ [[P (i, j, n) | i <- [ii..ii + 2], j <- [jj..jj + 2]] | 
              ii <- [1,4,7], jj <- [1,4,7], n <- [1..9]] -- Blocks

noMoreThanOne :: CNF (Int, Int, Int)
noMoreThanOne = [[neg x, neg y] | row <- atLeastOne, (x:xs) <- tails row, y <- xs]