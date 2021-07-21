module Test where

-- One way to interpret currying
add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)
--Add takes an int and returns a function which in turn takes an
-- Integer and returns an integer
--IN haskell it is permissible to define functions which takes functions
--  as arguements
--Ex:
twice :: (a -> a) -> a -> a
twice f x = f ( f x)

{-
Formally speaking a function that takes a function as an argument or returns a function as a 
result is called a higher order function.
The term is often used to describe taking functions as arguements.
-}

-- Using map and filter
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

{-
Foldr f v maps the empty list to the alue v and any nonempty list to the function f applied to the head of the list and then recursively processed tail.
Like -> 1:(2 : (3 : [])) Gives us-> 1+(1+(1+0))
-}

length1 :: [a] -> Int
length1 = foldr (\_ n -> 1+n) 0