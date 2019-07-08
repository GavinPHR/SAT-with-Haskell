import Data.List

-- Using the standard SAT input format
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
    -- SAT
    | e == [] = v   
    -- UNSAT
    | elem [] e = []    
    -- Test unit propagation
    | length units /= 0 = dpll (propagate usym e) (v ++ [usym])
    -- Propagate the first symbol
    | otherwise = dpll (propagate sym e) (v ++ [sym]) >||< dpll (propagate (-sym) e) (v ++ [(-sym)])
        where
            -- Get the unit clauses
            units = filter (\x -> (length x) == 1) e
            -- Get the symbol for unit propagation, only invoked if units is non-empty
            usym = head $ head units
            -- Get the first symbol
            sym = head $ head e
            -- Propagation helper: first delete clauses then remove opposite polarity
            propagate n e = map (\\ [-n]) $ filter (notElem n) e
            -- Acts like || 
            (>||<) x y = if x /= [] then x else y

