module Test where


concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs] 

-- Function to discard certain elements from a list
firstts :: [(a,b)] -> [a]
firstts ps = [x | (x,_) <- ps]

-- Testing guards on a positive factor function
factors' :: Int -> [Int]
factors' n = [x | x <- [1..n], n `mod` x == 0]
-- the comma would be the guard I imagine

-- Prime example
prime' :: Int -> Bool
prime' n = factors' n == [1,n]

-- List of primes example
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime' x]

-- Lookup table
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- Zip and pairs
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- Sorted Pairs
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- Checking to see number of spots in a list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']