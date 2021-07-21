module Test where

--import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, iterate)

-- Question 1 Show how the list comprehension can be reprocessed using the higher order functions map and filter
-- [f x | x <- xs, p x]
dummy :: (a -> Bool) -> (a -> b) -> [a] -> [b]
dummy p f xs = map f $ filter p xs

-- Question 2 Without looking at the definitions define the following higher order functions

-- A) decide if all elements of a list satisfy a predicate
all' :: (Int -> Bool) -> [Int] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs
-- makes it recursive

-- B) decide if any element of a list satisfies a predict
any' :: (Int -> Bool) -> [Int] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs
--practically the same as all but wit an or

-- C) Select elements from a list while they satify a predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []         = []
takeWhile' p (x:xs) 
           | p x       = x : takeWhile' p xs
           | otherwise  = []

-- D) Remove elements from a list while they satify a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []             = []
dropWhile' p xs@(x:xs')
            | p x           = dropWhile p xs'
            | otherwise     = xs

-- Question 3) redefine the functions map f and filter p using foldr
--map' :: (a -> [b]) -> [a] -> [a]
--map' f  = foldr (\x y -> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr(\x y -> if p x then x : y else y) []

-- Question 4) Using foldl define a function dec2int that converts a decimal number into an integer
dec2int :: [Int] -> Int
dec2int = foldl(\ys x -> ys*10 + x) 0

-- Question 5) Define a function curry that converts a function on pairs into a curried function and conversely the function uncrurry the converts a curried function with 2 args into a function on pairs
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y