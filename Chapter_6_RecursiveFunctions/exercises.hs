module Test where

-- Question 1: 
{-
Modify the definition of the factorial to prohibit negatives by adding a guard.
-}

--before guard
fac2 :: Integer -> Integer
fac2 0 = 1              -- Base Case
fac2 n = n * fac2 (n-1) -- Recursive Case
{- Without the negative guard we get a stack overflow-}
--after guard
facNeg :: Int -> Int
facNeg 0 = 1
facNeg n | n >= 1       = n * facNeg (n-1)
         | otherwise    = 0


-- Question 2
{-
define a recur func that returns the sum of the non neg int from give vale to zero ex sumdown 3 --> 3+2+1+0 = 6
-}

sumdown' :: Int -> Int
sumdown' n | n > 0      = n + sumdown' (n-1)
          | n == 0      = 0
          | n < 0       = n
         -- | otherwise = 0

-- Question 3 
{-
Define the exponentiation operator for non negative integers
-}

expon :: Int -> Int -> Int
expon 0 b = 0
expon a 0 = 1
expon a 1 = a
expon a b = a * (expon a (b - 1))

-- Question 4
{-
Define a recursive function euclid that implements the algo for calculating the GCD of 2 non neg ints
-}

euclids :: Int -> Int -> Int
euclids a 0 = a
euclids 0 b = b
euclids a b = euclids a ( b - a)

-- Question 5 
{-Show how drop init and length function-}

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs
-- [1,2,3] --> 1 + 1 +1

drop' :: Int -> [a] -> [a]
drop' 0 xs      = xs
drop' _ []      = []
drop' n (_:xs)  = drop' (n-1) xs
-- 2 [1,2,3,4]
-- 1 [2,3,4] 1 2 [3,4] --> [3,4]

init' :: [a] -> [a]
init' (x:xs) | null xs = []
            | otherwise = x : init' xs
--1: [2,3]
--1: 2: [3]
--1: 2: 3: []
--[1,2]

-- Question 6
{-Define the following libary funcs on lists using recursion-}
-- and
and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = if x == False
                then False
              else and1 xs
            
-- concat
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (xs:xss) = xs ++ concat1 xss
-- replicate
replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n-1) x
-- (!!)
(<!!>) :: [a] -> Int -> a
(<!!>) (x:xs) 0 = x
(<!!>) (x:xs) n = (<!!>) xs (n-1)
-- elem
elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 a (x:xs) = if a == x
                    then True
                 else elem1 a xs