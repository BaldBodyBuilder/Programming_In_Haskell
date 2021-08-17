module Test where

-- 12.1 Functors

inc :: [Int] -> [Int]
inc []  = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []  = []
sqr (n:ns) = n^2 : sqr ns

-- abstracts out into the map function
{-

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
Thus:
inc = map (+1)
sqr = map (^2)

Functor is taking parameterized types f and types a and appling them across a list

instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap = map

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just (g x)

User defined types as functors
data Tree a = Leaf a| Node (Tree a) (Tree a)
              deriving Show
    thus
instance Functor Tree where
    fmap g (Leaf x)     =   Leaf (g x)
    fmap g (Node l r)   =   Node (fmap g l) (fmap g r)
-}

-- type a is sometimes considered a container type
{-
instance Functor IO where
    fmap g mx = do {x <- mx; return (g x)}
-}

-- Why use functors?
{-
fmap can process elements of any structure that is functorial that is we can use the same name for functions that are essentially the same
rather than having to invent a separate name fo each instance 

We can also define generic functions that can be used with any functor
-}

-- Functor Laws:
{-
fmap id         = id --> f a = f a
fmap (g . h)    = fmap g . fmap h


-}

-- 12.2 Applicatives
{-
pure :: a -> f a

(<*>) :: f (a -> b) -> f a -> f b
eg g <*> f <*> z and its assumed to be left associateive

heirarchy of mapping functions
fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

pure and lists
>pure (+1) <*> [1,2,3]
    [2,3,4]


-}
prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys]
-- OR
prods1 :: [Int] -> [Int] -> [Int]
prods1 xs ys = pure (*) <*> xs <*> ys

-- IO
{-
Pure is the 'return' type for this function
<*> applies an impure function to an impure arguement to get an impure result
-}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

-- Applicative Laws!(4)
{-

pure id <*> x   = x
pure (g x)      = pure g <*> pure x
x <*> pure y    = pure (\g -> g y) <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-}

--12.3 Monads!
{-
>>= takes an argument of type a that may fail and a function of type a -> b whose result may fail and returns a result of type b that may fail
    Known as the bind function

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = eval x >>= \n
                 eval y >>= \m
                 safediv n m
redefined to:

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m
-}

instance Monad Maybe where
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x

instance Monad [] where
    xs >>= f = [y | x <- xs, y <- f x]

-- Function which defines all possible pairs

pairs :: [a] -> [b] [(a,b)]
pairs  xs ys = do x <- xs
                  y <- ys
                  return (x,y)

-- Relabelling Trees
data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b') (Leaf 'c'))

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _)     n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n')
                      where 
                          (l',n') = rlabel l n
                          (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)     = Leaf <$> fresh
alabel (Node l r)   = Node <$> alabel l <$> alabel r

mlabel' :: Tree a -> ST (Tree Int)
mlabel' (Leaf _)   = fresh >>= \n ->
                     return (Leaf n)
mlabel' (Node l r) = mlabel' l >>= \l' ->
                     mlabel' r >>= \r' ->
                     return (Node l' r')

-- Monad laws
return x >>= fresh  = f x
mx >>= return       = mx
(mx >>= f) >>= g    = mx >>= (\x -> (f >>= g))