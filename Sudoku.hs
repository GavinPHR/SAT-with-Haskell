import Encode hiding (CNF)
import DPLL
import Grid
import Data.List (sort)

solve'' :: Grid -> Valuation (Int, Int, Int)
solve'' g =  dpll (enc g) []


solve' :: Grid -> Valuation (Int, Int, Int)
solve' g = sort [n | n <- dpll (enc g) [], isPos n]
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