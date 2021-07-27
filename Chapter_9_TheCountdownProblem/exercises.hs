module Test where

-- Question 1)
{- redefine the combinatorial function choices using a list comprehension rather than using composition concat and map-}
--old
choices :: [a] -> [[a]]
choices = concat . map perms . subs

choices' :: [a] -> [[a]]
choices' xs = [ns | nss <- subs xs,
               ns <- perms yss]

-- Question 2)
{- Define a recur function that decides if one list is chosen from another without using perms or subs-}

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x []            = []
removeFirst x (y:ys)        | x == y = ys
                            | otherwise = y : removeFirst x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeFirst x ys)

-- Question 3) What effect would generalizing the function split to also return a pair of containing empty list on the behaviour of solutions
{-
It would eventually just get stuck in an infinite loop of evaling []
-}

-- Question 4) 
{-Using Choices Exprs and Eval verify that there are 33mlln possible expressions -}

-- All possible expr
possibleExprs :: [Int] -> [Expr]
possibleExprs = concat . map exprs . choices
-- Successful Expr
successfulExprs :: [Int] -> [[Int]]
successfulExprs = filter (not . null) . map eval . possibleExprs
--Size
totalPossible :: [Int] -> Int
totalPossible = length . possibleExprs
--Size
totalSuccessful :: [Int] -> Int
totalSuccessful = length . successfulExprs