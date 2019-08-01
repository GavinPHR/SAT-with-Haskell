module DPLL where
import Data.List

data Literal a = P a | N a deriving (Eq, Show, Ord)
type CNF a = [[Literal a]]
type Valuation a = [Literal a]

-- Negates a literal
neg :: Literal a -> Literal a
neg (P var) = N var
neg (N var) = P var

-- Takes an element out of a singleton list
the :: [a] -> a
the [x] = x
the _   = error "not singleton"


-- Input a CNF and a partial valuation, output a valuation if SAT otherwise []
dpll :: Eq a => CNF a -> Valuation a -> Valuation a
dpll e v
    -- SAT
    | e == [] = v   
    -- UNSAT
    | elem [] e = []    
    -- Unit propagate if there are unit clauses
    | units /= [] = dpll (propagate unitLit e) (v ++ [unitLit])
    -- Propagate the first literal
    | otherwise = dpll (propagate lit e) (v ++ [lit]) >||< dpll (propagate (neg lit) e) (v ++ [(neg lit)])
        where
            -- Get the unit clauses
            units = filter (\x -> (length x) == 1) e
            -- Get the literal for unit propagation, only invoked if units is non-empty
            unitLit = the $ head units
            -- Get the first literal
            lit = head $ head e
            -- Propagation helper: first delete clauses then remove opposite polarity
            propagate n e = map (\\ [neg n]) $ filter (notElem n) e
            -- Acts like || 
            (>||<) x y = if x /= [] then x else y


-- Example CNF
e :: CNF Int
e = [[N 1, P 2, P 3],
     [P 1, P 3, P 4],
     [P 1, P 3, N 4],
     [P 1, N 3, P 4],
     [P 1, N 3, P 4],
     [N 2, N 3, P 4],
     [N 1, P 2, N 3],
     [N 1, N 2, P 3]]
