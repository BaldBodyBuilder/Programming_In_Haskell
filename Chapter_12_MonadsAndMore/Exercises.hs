module Test where

-- Question 1: Define and instance of the functor class for the following type of binary trees
-- data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show
instance Functor Tree where
    fmap f (Node l n r) = Node (fmap g l) (fmap g n) (fmap g r)
    fmap _ Leaf         = Leaf

-- Question 2 Complete the following instance dec to makethe partially applied func into a functor

instance Functor ((->) a) where
    --fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap = (.)

--Question 3 Define an instance of the applicative class for the type a -> if you are famili you recog puire and <*>

instance Applicative ((->) a) where
    pure x = \_ -> x
    f <*> g = \x -> f x <*> g x

-- Question 4 There may be moer than one way to make a paramtereised type into an appluicative functor for ex. compelte the following decs that implement this idea:

newtype ZipList a = Z [a]
  deriving Show

instance Functor ZipList where
  fmap f (Z xs) = Z (fmap f xs)

instance Applicative ZipList where
  pure x = Z $ repeat x
  Z fs <*> Z xs = Z [f x | (f, x) <- zip fs xs]