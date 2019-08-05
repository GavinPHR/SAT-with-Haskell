module DPLL where
import Data.List ((\\))

data Literal a = P a | N a deriving (Eq, Show, Ord)
type Clause a = [Literal a]
type CNF a = [Clause a]
type Valuation a = [Literal a]

-- Negates a literal
neg :: Literal a -> Literal a
neg (P atom) = N atom
neg (N atom) = P atom

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
    | otherwise = dpll (propagate lit e) (v ++ [lit]) 
             >||< dpll (propagate (neg lit) e) (v ++ [(neg lit)])
        where
            -- Get the unit clauses
            units = filter (\x -> (length x) == 1) e
            -- Get the literal for unit propagation, only invoked if units is non-empty
            unitLit = head $ head units
            -- Get the first literal
            lit = head $ head e
            -- Propagation helper: first delete clauses then remove opposite polarity
            propagate n e = map (\\ [neg n]) $ filter (notElem n) e
            -- Acts like || 
            (>||<) x y = if x /= [] then x else y
 

-- Example CNF
e :: CNF Char
e = [[N 'A', P 'B', P 'C'],
     [P 'A', P 'C', P 'D'],
     [P 'A', P 'C', N 'D'],
     [P 'A', N 'C', P 'D'],
     [P 'A', N 'C', N 'D'],
     [N 'B', N 'C', P 'D'],
     [N 'A', P 'B', N 'C'],
     [N 'A', N 'B', P 'C']]
