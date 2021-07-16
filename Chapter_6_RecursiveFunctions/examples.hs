module Test where

fac' :: Int -> Int
fac' n = product [1..n]

-- Or

fac2 :: Int -> Int
fac2 0 = 1              -- Base Case
fac2 n = n * fac2 (n-1) -- Recursive Case

-- Recursion on Lists
product' :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' ns

length' :: [a] -> Int
length' []      = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' []      = []
reverse' (x:xs)  = reverse xs ++ [x]

insert' :: Ord a => a -> [a] -> [a]
insert' x []                = [x]
insert' x (y:ys) | x <= y   = x : y : ys
                 | otherwise = y : insert' x ys

isort :: Ord a => [a] -> [a]
isort []        = []
isort (x:xs)    = insert' x (isort xs)