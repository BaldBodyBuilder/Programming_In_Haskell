module Test where

import Data.Char
import System.IO
-- Homework questions for Ch. 10

-- Question 1)
{- redefine putstr using a list comprehension and library seq func

The original: 
putStr :: String -> IO ()
putStr []   = return ()
putStr (x:xs) = do putChar x
                   putStr xs

using:
sequence_ :: [IO a] -> IO ()
-}
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- Question 2)
{-
Using recursion define a version of putBoard that displays nim boards of any size rather than just 5 rows.
Hint: Use and aux func for current row

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

-}
type Board = [Int]
-- Aux function
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

putBoard = putBoard' 1

putBoard' :: Board -> IO ()
putBoard' r [] = return [] --Empty board
putBoard' r (x:xs) = do putRow r x
                        putBoard' (r+1) ns
-- putBoard needs to accept an arbitrary number and then recursively create rows 

-- Question 3) 
{- Redefine the first putboard  using a list compre and sequence_-}

putBoard'' :: Board -> IO ()
putBoard'' b = sequence_ [putRow r n | (r,n) <- zip [1..] b]
