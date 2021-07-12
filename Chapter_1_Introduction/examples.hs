--Summing Numbers, Example 1
module Test where

sum1 []     = 0
sum1 (n:ns) = n + sum1 ns