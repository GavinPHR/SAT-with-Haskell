module NQueens where
import DPLL
import Data.List (sort, tails)

solve' :: Int -> Valuation (Int, Int)
solve' n = sort [n | n <- dpll (encode n) [], isPos n]
           where
            isPos (P _) = True
            isPos (N _) = False

format :: Int -> Valuation (Int, Int) -> [[Char]]
format n [] = []
format n (P (l,r):vs) = (['X' | x <- [1..(r-1)]] ++ ['Q'] ++ ['X' | x <- [(r+1)..n]]):format n vs

solve :: Int -> IO()
solve n = mapM_ putStrLn $ format n $ solve' n

encode :: Int -> CNF (Int, Int)
encode n = rowAndCOl n ++ noSameDiag n

rowAndCOl :: Int -> CNF (Int, Int)
rowAndCOl n = let
                atLeastOne = [[P (i,j) | j <- [1..n]] | i <- [1..n]] -- Row
                          ++ [[P (i,j) | i <- [1..n]] | j <- [1..n]] -- Column
                noMoreThanOne = [[neg x, neg y] | row <- atLeastOne, (x:xs) <- tails row, y <- xs]
              in 
                noMoreThanOne ++ atLeastOne 
                
noSameDiag :: Int -> CNF (Int, Int)
noSameDiag n = [[N (d+j,j), N (d+k,k)] | 
                d <- [0..(n-2)], j <- [1..(n-d)], k <- [(j+1)..(n-2)]]
            ++ [[N (j,j-d), N (k,k-d)] | 
                d <- [(-(n-2))..(-1)], j <- [1..(n+d)], k <- [(j+1)..(n+d)]]
            ++ [[N (j,d-j), N (k,d-k)] | 
                d <- [3..(n+1)], j <- [1..(d-1)], k <- [(j+1)..(d-1)]]
            ++ [[N (j,d-j), N (k,d-k)] | 
                d <- [(n+2)..(2*n-1)], j <- [(d-n)..n], k <- [(j+1)..(d-1)]]
            