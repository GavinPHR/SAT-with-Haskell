import Encode hiding (CNF)
import DPLL
import Data.List (sort)

solve' :: Grid -> Valuation
solve' g = sort [n | n <- dpll (enc g) [], n > 0]

format :: Valuation -> Grid
format [] = []
format v  = let
                (l, r) = splitAt 9 v
            in
                map (`mod` 10) l : format r

solve :: Grid -> IO ()
solve g = mapM_ print $ format $ solve' g


evilGrid :: Grid
evilGrid = [[0,0,0, 0,8,7, 0,2,0],
            [8,0,7, 0,0,0, 0,0,0],
            [3,0,0, 5,0,0, 4,0,0],

            [0,0,0, 7,0,0, 0,5,4],
            [0,0,0, 4,0,9, 0,0,0],
            [6,9,0, 0,0,3, 0,0,0],

            [0,0,2, 0,0,4, 0,0,7],
            [0,0,0, 0,0,0, 5,0,6],
            [0,4,0, 1,7,0, 0,0,0]]

tGrid :: Grid
tGrid = [[5,3,0, 0,7,0, 0,0,0],
         [6,0,0, 1,9,5, 0,0,0],
         [0,9,8, 0,0,0, 0,6,0],

         [8,0,0, 0,6,0, 0,0,3],
         [4,0,0, 8,0,3, 0,0,1],
         [7,0,0, 0,2,0, 0,0,6],

         [0,6,0, 0,0,0, 2,8,0],
         [0,0,0, 4,1,9, 0,0,5],
         [0,0,0, 0,8,0, 0,7,9]]


