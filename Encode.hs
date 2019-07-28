module Encode where
import Data.List

type Grid = [[Int]]
type CNF = [[Int]]

testGrid :: Grid
testGrid = [[2,0,0, 6,0,0, 0,0,7],
            [3,0,5, 0,9,0, 0,2,4],
            [0,0,8, 0,0,0, 5,0,9],

            [5,0,0, 9,0,7, 0,0,0],
            [0,0,2, 5,8,4, 7,0,0],
            [0,0,0, 3,0,6, 0,0,1],

            [8,0,4, 0,0,0, 1,0,0],
            [6,2,0, 0,4,0, 9,0,5],
            [9,0,0, 0,0,5, 0,0,8]]

emptyGrid :: Grid
emptyGrid = [[0,0,0, 0,0,0, 0,0,0],
             [0,0,0, 0,0,0, 0,0,0],
             [0,0,0, 0,0,0, 0,0,0],

             [0,0,0, 0,0,0, 0,0,0],
             [0,0,0, 0,0,0, 0,0,0],
             [0,0,0, 0,0,0, 0,0,0],

             [0,0,0, 0,0,0, 0,0,0],
             [0,0,0, 0,0,0, 0,0,0],
             [0,0,0, 0,0,0, 0,0,0]]

-- If a cell (i, j) contains the number n, then ijn, else -ijn
-- http://www.cs.qub.ac.uk/~I.Spence/SuDoku/SuDoku.html

enc :: Grid -> CNF
enc g = prefilled g ++ encCll ++ encRow ++ encCol ++ encBlk

-- Pre-filled cells
prefilled :: Grid -> CNF
prefilled g = [[(read (show i ++ show j ++ show n) :: Int)] | 
               (i, row) <- zip [1..] g,
               (j, n) <- zip [1..] row, 
               n /= 0]

-- Below encoding stays the same for all grids

-- Cell
encCll :: CNF
encCll = let
            a = [[read (show i ++ show j ++ show n) :: Int | n <- [1..9]]
                 | i <- [1..9], j <- [1..9]]
            b = [[-x, -y] | row <- a, (x:xs) <- tails row, y <- xs]
         in
            a ++ b

-- Row
encRow :: CNF
encRow = let
            a = [[read (show i ++ show j ++ show n) :: Int | j <- [1..9]]
                 | i <- [1..9], n <- [1..9]]
            b = [[-x, -y] | row <- a, (x:xs) <- tails row, y <- xs]
         in
            a ++ b

-- Column
encCol :: CNF
encCol = let
            a = [[read (show i ++ show j ++ show n) :: Int | i <- [1..9]]
                 | j <- [1..9], n <- [1..9]]
            b = [[-x, -y] | row <- a, (x:xs) <- tails row, y <- xs]
         in
            a ++ b

-- Block
encBlk :: CNF
encBlk = let
            a = [[read (show i ++ show j ++ show n) :: Int | i <- [ib..ib + 2], j <- [jb..jb + 2]] 
                 | ib <- [1,4,7], jb <- [1,4,7], n <- [1..9]]
            b = [[-x, -y] | row <- a, (x:xs) <- tails row, y <- xs]
         in 
            a ++ b



 
