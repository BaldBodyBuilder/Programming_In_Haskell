module Test where

{-
Given a sequence of numbers and a target number attempt to construct an expression whose value is the target
by combining one or more number from the sequence using addition subtract mult and divison and parenthesis.
-}

-- 9.2 Arithmetic Operators

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- 9.3 Numeric Expressions

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
    show (Val n)            = show n
    show (App o l r)        = brak l ++ show o ++ brak r
                              where 
                                  brak (Val n) = show n
                                  brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)  = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)  = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                    y <- eval r,
                                    valid o x y]

-- 9.4 Combinatorial Functions

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
            where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []    = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- All possible ways of selecting a grouping of numbers
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- Formalizes what it means to solve and instance of the countdown problem
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
    elem (values e) (choices ns) && eval e == [n]

-- 9.6 Brute Force Solution
-- Splits a list into all possible ways to split into two non-empty lists that append to the original
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- Now we can define the key function exprs that returns all possible solutions to a given expression in the list
exprs :: [Int] ->  [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
            l   <- exprs ls,
            r   <- exprs rs,
            e   <- combine l r]
-- Combining each expression
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- Aux. function to combine the 4 functions
ops :: [Op]
ops = [Add,Sub,Mul,Div]

--Solutions returns all possible expression that solve an instance of the problem
solutions :: [Int] -> Int -> [Expr]
solutions ns n =
    [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

main :: IO ()
main = print (solutions' [1,3,5,7,10,25,50] 765)

type Result = (Expr, Int)
-- All possible results combining expressions whose list of values is precisely a given list
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                    lx   <- results ls,
                    ry   <- results rs,
                    res   <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = 
    [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
    [e | ns' <- choices ns, (e,m) <- results ns', m == n]