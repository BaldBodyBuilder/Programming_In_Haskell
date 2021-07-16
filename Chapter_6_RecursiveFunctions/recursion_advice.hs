--product' :: [Int] -> Int
product' :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' ns

--or

product2' :: Num a => [a] -> a
product2'  = foldr (*) 1

-- Example 2

drop' :: Int -> [a] -> [a]
drop' 0 xs      = xs
drop' _ []       = []
drop' n (_:xs)   = drop' (n-1) xs


-- Example 3
--Removes the last element from a non-negative list 
init' :: [a] -> [a]
init' (x:xs) | null xs = []
            | otherwise = x : init' xs
