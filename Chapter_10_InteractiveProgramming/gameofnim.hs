module Test where

import Data.Char -- Used for the digitToInt

-- Example for the Game of Nim
-- Game is initially setup as follows:
{-
1: * * * * *
2: * * * *
3: * * *
4: * *
5: *
Building from the bottom up.
Goal is for a player (2) to be the first to remove all stars from the board.
-}

-- We represent the player number 1 or 2 with an int an use the following function to give the next player
next :: Int -> Int
next 1 = 2
next 2 = 1

--  We represent the board as a list comprising the number of stars on each row 
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

-- A move in the game is specified by a row number and the number of stars to be removed and is valid if the row contains at least this many stars:
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num
-- Subtraction is used in the above because indexing for lists starts at 0

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <-zip [1..] board]
    where update r n = if r == row then n-num else n
{-
Example:
a move of initial 1 3 returns a new board [2,4,3,2,1]
-}

-- Defining a function that displays a row of the board on the screen and given the row number  and remaining stars
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

-- Final full put board function to load the board
putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

-- Displays a prompt that reads a single character from the keyboard 
getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newLine
                     if isDigit x then
                         return (digitToInt x)
                     else 
                         do putStrLn "Error: Invalid Digit "
                            getDigit prompt

newLine :: IO ()
newLine = putChar '\n'

play :: Board -> Int -> IO ()
play board player = 
    do newLine
       putBoard board
       if finished board then
           do newLine 
              putStr "Player "
              putStr (show (next player))
              putStrLn " wins!!"
        else
            do newLine
               putStr "Player "
               putStrLn (show player)
               row <- getDigit "Enter a row number: "
               num <- getDigit "Stars to remove : "
               if valid board row num then
                   play (move board row num) (next player)
               else
                   do newLine
                      putStrLn "ERROR: Invalid move"
                      play board player

nim :: IO ()
nim = play initial 1
