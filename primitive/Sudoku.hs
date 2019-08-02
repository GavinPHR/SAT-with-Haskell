import Encode
import DPLL
import Grid
import Data.List (sort)

main = solve xtremGrid 

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