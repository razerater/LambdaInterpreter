import Data.List
import PA1Helper
import System.Environment (getArgs)

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp Lexp

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

atoms = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
         "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]



-- BIG FUNCTIONS
-- the big boys

-- The lambda calculus reducer. Recursively applies itself on components of a lambda expression
-- until it is applied to something which cannot be reduced (an atom)
reducer :: Lexp -> Lexp
reducer (Lambda a e) = eta_reduce (Lambda a (reducer e))
reducer (Apply e f) = beta_reduce (alpha_rename (Apply (reducer e) (reducer f)))
reducer lexp = lexp -- atom


-- Alpha renaming
-- Rename all variables in e that would be bound in lexp with a new, yet-unused variable name
alpha_rename :: Lexp -> Lexp
alpha_rename lexp@(Apply e f) = (Apply (alpha_helper e (vars_unused lexp)) f)
alpha_rename lexp = lexp

-- Helper to iteratively rename variables in the lhs
alpha_helper :: Lexp -> [String] -> Lexp
alpha_helper lexp@(Lambda a e) list@(v:vs) =
    Lambda v (alpha_helper (replace (Atom a) e (Atom v)) vs)
alpha_helper lexp _ = lexp


-- Eta reduction
-- A lambda calculus of the form \x.(y x) can be reduced to y if x is free in y
eta_reduce :: Lexp -> Lexp
eta_reduce lexp@(Lambda a (Apply b (Atom c))) = if (a == c) && (bound_in b a)
                                                then (eta_reduce b)
                                                else lexp
eta_reduce lexp = lexp


-- Beta reduction
-- For a lambda calculus of the form (\x.y z), replace all instances of x in y with z
beta_reduce :: Lexp -> Lexp
beta_reduce (Apply (Lambda a e) f) = replace (Atom a) e f
beta_reduce lexp = lexp



-- HELPER FUNCTIONS
-- the little boys (not all of these ended up being used, but were still a good exercise)

-- Return true if a is free in lexp
free_in :: Lexp -> String -> Bool
free_in lexp a = elem a (free_vars lexp)

-- Return true if a is bound in lexp (negation of free_in)
bound_in :: Lexp -> String -> Bool
bound_in lexp a = not (free_in lexp a)

-- Get all free variables in an expression
free_vars :: Lexp -> [String]
-- free_vars lexp = (free_vars lexp) \\ (bound_vars lexp)
free_vars (Atom a) = [a]
free_vars (Lambda a e) = filter (/= a) (free_vars e)
free_vars (Apply e f) = (free_vars e) ++ (free_vars f)

-- Get all bound variables in an expression
bound_vars :: Lexp -> [String]
bound_vars (Atom a) = []
bound_vars (Lambda a e) = [a] ++ (bound_vars e)
bound_vars (Apply e f) = (bound_vars e) ++ (bound_vars f)

-- Get all variables in a given lambda calculus
vars :: Lexp -> [String]
vars (Atom a) = [a]
vars (Lambda a e) = [a] ++ vars e
vars (Apply e f) = vars e ++ vars f

-- Get all variables (atoms) that aren't used in a given lambda calculus
vars_unused :: Lexp -> [String]
vars_unused lexp = atoms \\ (vars lexp)

-- Replace all instances of old in e with new
replace :: Lexp -> Lexp -> Lexp -> Lexp
replace old a@(Atom _) new = if old == a then new else a
replace old (Lambda a e) new@(Atom b) = if old == (Atom a)
                                        then Lambda b (replace old e new)
                                        else Lambda a (replace old e new)
replace old (Lambda a e) new = Lambda a (replace old e new)
replace old (Apply e f) new = Apply (replace old e new) (replace old f new)



-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    runProgram inFile outFile reducer
