module Test where

zip' :: [a] -> [b] -> [(a,b)]
zip' []      _       = []
zip' _      []       = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs      = xs
drop' _ []      = []
drop' n (_:xs)  = drop' (n-1) xs

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-2) + fibo (n-1)

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)