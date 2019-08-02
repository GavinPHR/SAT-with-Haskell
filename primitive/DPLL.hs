module DPLL where
import Data.List

type CNF = [[Int]]
type Valuation = [Int]

-- Input a CNF and a partial valuation, output a valuation if SAT otherwise []
dpll :: CNF -> Valuation -> Valuation
dpll e v
    -- SAT
    | e == [] = v   
    -- UNSAT
    | elem [] e = []    
    -- Unit propagate if there are unit clauses
    | units /= [] = dpll (propagate unitLit e) (v ++ [unitLit])
    -- Propagate the first literal
    | otherwise = dpll (propagate lit e) (v ++ [lit]) >||< dpll (propagate (-lit) e) (v ++ [(-lit)])
        where
            -- Get all the unit clauses
            units = filter (\x -> (length x) == 1) e
            -- Get the literal for unit propagation, only invoked if units is non-empty
            unitLit = head $ head units
            -- Get the first literal
            lit = head $ head e
            -- Propagation helper: first delete clauses then remove opposite polarity
            propagate n e = map (\\ [-n]) $ filter (notElem n) e
            -- Acts like || 
            (>||<) x y = if x /= [] then x else y


-- Example CNF
e :: CNF
e = [[-1,  2,  3],
     [ 1,  3,  4],
     [ 1,  3, -4],
     [ 1, -3,  4],
     [ 1, -3,  4],
     [-2, -3,  4],
     [-1,  2, -3],
     [-1, -2,  3]]
