module Test where

-- Question 1: give an expression that calculates the sum 1^2 + ...100^2
sqrSum :: Int -> Int
sqrSum n = sum [x^2 | x <- [1..n]]

-- Question 2: Coordinate grid of mxn for all pairs x,y
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Question 3: Using grid and LC, define a function that returns a coord sqare of size n excluding the diagonal
squareGrid :: Int -> [(Int,Int)]
squareGrid n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Question 4: Similar to length show the library function replicate produces a list of iden ele 
--eg replicate 3 True
replicate' :: Int -> a -> [a]
replicate' m n = [n | _ <- [0..m-1]]

-- Question 5: A triple of pos integers define a pythag func with 3 gens to return list of all such triples with given limit
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n],
                    y <- [1..n],
                    z <- [1..n],
                    (x^2) + (y^2) == (z^2)]

-- Question 6: A positive integer is perfect if it equals the sum of its factors
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum ( factors x ) - x == x ]