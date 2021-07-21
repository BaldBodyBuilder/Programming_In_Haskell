module Test where


type Assoc k v = [(k,v)]
-- We develop a function that decides if simple logical propositions are always true --> Tautologies
data Prop = Const Bool
        | Var Char
        | Not Prop
        | And Prop Prop
        | Imply Prop Prop

-- Four Basic Propositions
-- Proposition variables
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
        (Var 'A') (Var 'B'))) (Var 'B')

-- Associates var names to lookup values
type Subst = Assoc Char Bool

-- Patten matching for with substitution
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s ( Var x )    = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Imply p q)  = eval s p <= eval s q

-- Returns a list of all props as variables
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
{-
bools n = map( reverse . map conv . make n . int2bin) range
        where 
            range       =[0..(2^n)-1]
            make n bs   = take n ( bs ++ repeat 0)
            conv 0      = False
            conv 1      = True
-}
bools n = nah bss ++ yes bss
  where 
    -- Place false in front of each list in the first copy
    nah = map (False:)
    -- Place true in front of each list in the second copy
    yes = map (True:)
    bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups $ vars p

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- Still getting a stack overflow with isTaut
