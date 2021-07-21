module Test where

-- 8.1 Type declarations

--type String = [Char]
type Pos = (Int, Int)
type Trans = Pos -> Pos

-- type Tree = (Int,[Tree]) // Won't work because its recursive

type Pair a = (a,a)

type Assoc k v = [(k,v)] 

find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k', v) <- t, k==k']

-- 8.2 Data Declarations

-- data Bool = False | True
-- Could also be written as data kek = A | B | C
data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves []        p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float 

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- data Maybe a = Nothing | Just a

-- Defining safe versions of div and head
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- 8.3 Newtype declarations
--newtype Nat  = N Int

-- 8.4 Recursive Types
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat 
add zero  n = n
add (Succ m) n = Succ (add m n)

-- Another Example
data List a = Nil | Cons a ( List a)

len :: List a -> Int
len Nil     = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))
-- Does a given value occur in a tree?
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == ys
occurs x (Node l y r)   = x == y || occurs x l || occurs x r

--flattening a tree 
flatten :: Tree a -> [a]
flatten (Leaf x)        = [x]
flatten (Node l x r)    = flatten l ++ [x] ++ flatten r

-- 8.5 Class and Instance Declarations
{-
Derived instances
data Bool = False | True
            deriving (Eq, Ord, Show, Read)

-}