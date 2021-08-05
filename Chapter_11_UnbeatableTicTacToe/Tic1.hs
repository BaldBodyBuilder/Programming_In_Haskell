module Tic1 where

import Data.Char
import Data.List
import System.IO

-- Rather than assume the grid must be a 3x3 we will allow it to be modifiable
size :: Int
size = 3

type Grid = [[Player]]

--Player is either 0 B X with B as blank

data Player = O | B | X
            deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid Utils

empty :: Grid
empty = replicate size (replicate size B)

-- If a grid is full all values should be non-blanc

full :: Grid -> Bool
full = all (/= B) . concat

-- Deciding whose turn it is:

turn :: Grid -> Bool
turn g = if os <= xs then O else X
    where 
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)
        ps = concat g

-- Assumes player O goes first ^

-- Has the game been won?

wins :: Player -> Grid -> Bool
wins p g = any line  (rows ++ cols ++ dias)
           where
               line = all ( == p)
               rows = g
               cols = transpose g
               dias = [diag g, diag (map reverse g)]

diag :: Grid -> Bool
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Grid
won g = wins O g || wins X g 

-- Displaying the grid

putGrid :: Grid -> IO ()
putGrid = 
    putStrln . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
              beside = foldr1 (zipWith (++))
              bar    = replicate 3 "|"
--foldr1 is the same as foldr but can only be applied to non-empty lists while Zipwith behaves in teh way as zip but applies a given function to each pair of values in the list

showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer B = ["   ","   ","   "]
showPlayer X = ["   "," X ","   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- Making a move
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i< size^2 && concat g !! i == B

-- Applies a move to the grid

move :: Grid -> Int -> Player -> [Grid]
move g i p = 
    if valid f i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Reading a number
getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                       return (read xs)
                   else 
                       do putStrLn "Error: Invalid Number"
                          getNat prompt

-- Human v. Human
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "Nobody wins!\n"
         | otherwise =
             do i <- getNat (prompt p)
                case move g i of
                    [] -> do putStrLn "Error: invalid move!"
                             run' g p
                    [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "player " ++ show p ++ ", enter your move: "

-- Game Trees

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
   | won g      = []
   | full g     = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

-- Pruning the tree

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> (Grid,Player)
minimax (Node g [])
  | wins O g = Node (g,O) []
  | wins X g = Node (g,X) []
  | otherwise = Node (g,B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
                  where 
                      ts' = map minimax ts
                      ps = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where 
                   tree = prune depth (gametree g p)
                   Node (_, best) ts = minimax tree


main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p
play' :: Grid -> Player -> IO ()
play' g p
   | wins O g = putStrLn " Player O wins!\n"
   | wins X g = putStrLn " Player O wins!\n"
   | full g = putStrLn " Its a draw\n"
   | p == O = do i <- getNat (prompt p)
                case move g i p of
                    [] -> do putStrLn "Error: Invalid Move"
                            play' g p
                    [g'] -> play g' (next p)
   | p == X = do putStr "Player X is thinking ..."
                 (play $! (bestmove g p)) (next p)
{-

(X + A*F/G) * (Y-B) = X * Y
X(Y-B) + A*F/G(Y-B) = X * Y
X*Y - X*B + A*F/G(Y) - A*F/G(B) = X * Y
-X*B + A*F/G(Y) - A*F/G(B) = 0
A*F/G(Y) - A*F/G(B) = X*B
A*F/G(Y) = X*B + A*F/G(B)
A*F*Y = G*(X*B + A*F/G(B))
A*F*Y >= B*(G*X + A*F)
-}