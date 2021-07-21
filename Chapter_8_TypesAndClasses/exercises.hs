module Test where

-- Question 1) In a similar manner to the function add define a recursive multiplication function mult for the recursive type of nat 
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat 
add Zero  n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult m (Succ n) = add m (mult m n)

-- Question 2) Using the function redefine the function occurs for search trees why is this new def more efficient than the original.
{-
data Ordering = LT | EQ | GT

compare :: Ord a => a -> a -> Ordering
-}
--occurs :: Ord a => a -> Tree a -> Bool

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)                 = x == y
occurs x (Node l y r) | x == y    = True
                      | x < y     = occurs x l
                      | otherwise = occurs x r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)     = x == y 
occurs' x (Node l y r) = case compare x y of 
                           LT -> occurs' x l
                           EQ -> True
                           GT -> occurs' x r
-- So the second occurs is more efficient because we only HAVE to make one comparison, the first may require 2

-- Question 3) Binary Trees, define a function to see if a tree is balanced or not.
-- NUmber of leaves on the left and right side of a tree must be equal for it to be balanced.
data BalTree a = BalLeaf a | BalNode (BalTree a) (BalTree a) deriving Show

leafLen :: BalTree a -> Int
leafLen (BalLeaf x)      = 1
leafLen (BalNode l r)    = leafLen l + leafLen r

bal :: BalTree a -> Bool
bal (BLeaf x) = True
bal (BalNode l r) = abs (leafLen l - leafLen r) <= 1 && bal l && bal r

--alternatively
leaves (BalLeaf _)  = 1
leaves (BalNode l r) = leaves l + leaves r

bal' :: BalTree a -> Bool
bal' (BalLeaf _) = True
bal' (BalNode l r) = abs (leaves l - leaves r) <=
                     && bal' l && bal' r

-- Question 4) Define a balance function that converts a non-empty list into a balanced tree first define a halving function

splitList :: [a] -> ([a], [a])
splitList xs = ((take n xs), (drop n xs))
  where
    n = length xs `div` 2

balanced :: [a] -> BalTree a
balanced [x] = BalLeaf x
balanced xs  =
  BalNode 
    (balanced (fst (splitList xs)))
    (balanced (snd (splitList xs)))