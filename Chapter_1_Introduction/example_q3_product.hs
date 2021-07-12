module Test where

product1 []     = 1
product1 (n:ns) = n * product1 ns