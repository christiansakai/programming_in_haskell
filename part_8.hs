-- Code
type Pos        = (Int, Int)
type Trans      = Pos -> Pos
type Pair a     = (a, a)
type Assoc k v  = [(k, v)]

find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k', v) <- t, k == k']

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y)  = (x + 1, y)
move West (x, y)  = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves []     p  = p
moves (m:ms) p  = moves ms (move m p)

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

safediv :: Int -> Int -> Maybe Int
safediv _ 0     = Nothing
safediv m n     = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead []   = Nothing
safehead xs   = Just (head xs)

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero      = 0
nat2int (Succ n)  = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0       = Zero
int2nat n       = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n      = n
add (Succ m) n  = Succ (add m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil           = 0
len (Cons _ xs)   = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)      = [x]
flatten (Node l x r)  = flatten l ++ [x] ++ flatten r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)      = x == y
occurs' x (Node l y r)
  | x == y              = True
  | x < y               = occurs x l
  | otherwise           = occurs x r

-- Tautology Checker
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

-- find :: (Eq a, Bool b) => a -> Subst -> b
-- find x (Assoc a b) = b

-- eval :: Subst -> Prop -> Bool
-- eval _ (Const b)    = b
-- eval s (Var x)      = find x s
-- eval s (Not p)      = not (eval s p)
-- eval s (And p q)    = eval s p && eval s q
-- eval s (Imply p q)  = eval s p <= eval s q

-- vars :: Prop -> [Char]
-- vars (Const _)    = []
-- vars (Var x)      = [x]
-- vars (Not p)      = vars p
-- vars (And p q)    = vars p ++ vars q
-- vars (Imply p q)  = vars p ++ vars q

-- bools :: Int -> [[Bool]]
-- bools 0 = [[]]
-- bools n = map (False:) bss ++ map (True:) bss
--           where bss = bools (n - 1)

-- rmdups :: Eq a => [a] -> [a]
-- rmdups []     = []
-- rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- substs :: Prop -> [Subst]
-- substs p = map (zip vs) (bools (length vs))
--            where vs = rmdups (vars p)

-- Abstract Machine
data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value e = eval e []

data Op = EVAL Expr | ADD Int
type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

-- Exercises
-- No. 1
mult :: Nat -> Nat -> Nat
mult m Zero     = Zero
mult m (Succ n) = add m (mult m n)

-- No. 2
occurs'' :: Ord a => a -> Tree a -> Bool
occurs'' x (Leaf y)     = x == y
occurs'' x (Node l y r) = 
  case compare x y of
    LT -> occurs x l
    EQ -> True
    GT -> occurs x r

-- No. 3
data Treee a = Leeaf a | Nodee (Treee a) (Treee a)
               deriving Show

countLeaves :: Treee a -> Int
countLeaves (Leeaf _)     = 1
countLeaves (Nodee l r)    = countLeaves l + countLeaves r

balanced :: Treee a -> Bool
balanced (Leeaf _)    = True
balanced (Nodee l r)  = abs (leavesLeft - leavesRight) <= 1
                        where leavesLeft = countLeaves l
                              leavesRight = countLeaves r

-- No. 4
halve :: [a] -> ([a], [a])
halve xs
  | odd (length xs) = (take ((length xs `div` 2) + 1) xs, drop (length xs - (length xs `div` 2)) xs)
  | otherwise       = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

balance :: [a] -> Treee a
balance [x]     = Leeaf x
balance xs      = Nodee (balance left) (balance right)
                  where (left, right) = halve xs

-- No. 5

-- No. 6

-- No. 7

-- No. 8

