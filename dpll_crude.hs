import Data.List

e :: [[Int]]
e = [[-1,  2,  3],
     [ 1,  3,  4],
     [ 1,  3, -4],
     [ 1, -3,  4],
     [ 1, -3, -4],
     [-2, -3,  4],
     [-1,  2, -3],
     [-1, -2,  3]]

dpll :: [[Int]] -> [Int] -> [Int]
dpll e v
    | e == []   = v
    | elem [] e = []
    | length units /= 0 =
        dpll (propagate usym e) (v ++ [usym])
    | dpll (propagate sym e) (v ++ [sym]) /= [] = dpll (propagate sym e) (v ++ [sym])
    | otherwise = dpll (propagate (-sym) e) (v ++ [(-sym)])
        where
            units = filter (\x -> (length x) == 1) e
            usym = head $ head units
            sym = head $ head e
            propagate n e = map (\\ [-n]) $ filter (notElem n) e

