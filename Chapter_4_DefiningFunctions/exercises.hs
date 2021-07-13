module Test where

-- Question 1 using library functions define a function to split a list into two halves
halve' :: [a] -> ([a],[a])
halve' ns = if ((length ns) `mod` 2) == 0 then splitAt ((length ns) `div` 2) ns else ([],[])
--using the splitAt and div functions

-- Question 2 define a function third that returns the third ele in a list
third :: [a] -> a
third ns = head ( tail ( tail ns))
--or
-- third xs = xs !! 2 grabs the second (third) index [0,1,2]
-- third (_:_:x:_) = x

-- Question 3 function `safetail` behaves like tail, use tail and null
safetail :: [a] -> [a]
safetail ns = if null ns
                then ns
              else if (length ns) == 1 
                then [] 
              else tail ns
-- or
-- safetail xs | null xs = []
--             | otherwise = tail xs
-- safetail [] = []
-- safetail (_:xs) = xs

--Question 4 show how the disjunction operator can be defined 4 different ways
{-
T || T = T
T || F = T
F || F = F
F || T = T

F || F = F
_ || _ = T

F || B = B
T || _ = T
-}

-- Question 7 formalize mult
mult :: Integer -> (Integer -> (Integer -> Integer))
mult = \x -> (\y -> (\z -> x * y * z))
--mult x y z = x*y*z