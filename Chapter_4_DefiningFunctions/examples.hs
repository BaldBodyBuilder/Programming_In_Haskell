module Test where


splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

abs' n | n >= 0     = n
       | otherwise  = -n

signum' n | n < 0 = -1
          | n == 0  = 0
          | otherwise = 1

testCons :: [Char] -> Bool
testCons ('a' : _) = True
testCons _ = False

addLambda :: Int -> Int -> Int
addLambda = \x -> (\y -> x + y)


odds':: Int -> [Int]
odds' n = map (\x -> x*2 + 1) [0..n-1]