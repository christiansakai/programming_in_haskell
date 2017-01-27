import Data.Char

-- Code
-- 12.1 Functors
inc :: [Int] -> [Int]
inc []      = []
inc (n:ns)  = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr []      = []
sqr (n:ns)  = n^2 : sqr ns

map' :: (a -> b) -> [a] -> [b]
map' f []      = []
map' f (x:xs)  = f x : map' f xs

inc' :: [Int] -> [Int]
inc' = map' (+1)

sqr' :: [Int] -> [Int]
sqr' = map' (^2)

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+1)

-- 12.2 Applicatives
prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x * y | x <- xs, y <- ys]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' []     = pure []
sequenceA' (x:xs) = pure (:) <*> x <*> sequenceA' xs

getChars' :: Int -> IO String
getChars' n = sequenceA' (replicate n getChar)

-- 12.3 Monads
data Expr = Val Int 
          | Div Expr Expr

eval :: Expr -> Int
eval (Val n)    = n
eval (Div x y)  = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0   = Nothing
safediv m n   = Just (m `div` n)

eval' :: Expr -> Maybe Int
eval' (Val n)    = Just n
eval' (Div x y)  =
  case eval' x of 
    Nothing -> Nothing
    Just n  -> case eval' y of
                 Nothing -> Nothing
                 Just m   -> safediv n m

eval'' :: Expr -> Maybe Int
eval'' (Val n)    = Just n
eval'' (Div mx my)  = 
  eval'' mx >>= (\x ->
    eval'' my >>= (\y ->
      safediv x y))

eval''' :: Expr -> Maybe Int
eval''' (Val n)     = Just n
eval''' (Div mx my) = do
  x <- eval''' mx
  y <- eval''' my
  safediv x y

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

pairs' :: [a] -> [b] -> [(a, b)]
pairs' xs ys = [(x, y) | x <- xs, y <- ys]

pairs'' :: [a] -> [b] -> [(a, b)]
pairs'' xs ys = 
  xs >>= (\x ->
    ys >>= (\y ->
      return (x, y)))

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

-- This is from the book, but confusing for me because of app function
-- instance Functor ST where
--   -- fmap :: (a -> b) -> ST a -> ST b
--   fmap g st = S (\s -> 
--     let (x, s') = app st s
--      in (g x, s'))

-- So I write this, bypassing app
instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f (S sta) = S stb
    where stb state = let (a, state') = sta state
                          in (f a, state')

-- This is from the book, but confusing for me because of app function
-- instance Applicative ST where
--   -- pure :: a -> ST a
--   pure x = S (\s -> (x, s))

--   -- (<*>) :: ST (a -> b) -> ST a -> ST b
--   stf <*> stx = S (\s ->
--     let (f, s') = app stf s
--         (x, s'') = app stx s' 
--      in (f x, s''))

-- So I write this, bypassing app
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S sta
    where sta state = (x, state)

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  (S stf) <*> (S sta) = S stb
    where stb state = let (f, state')   = stf state
                          (a, state'')  = sta state'
                       in (f a, state'')

-- This is from the book, but confusing for me because of app function
-- instance Monad ST where
--   -- (>>=) :: ST a -> (a -> ST b) -> ST b
--   st >>= f = S (\s -> 
--     let (x, s') = app st s
--      in app (f x) s')

-- So I write this, bypassing app
instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  (S sta) >>= f = S stb
    where stb state = let (a, state')   = sta state
                          (S stb)       = f a
                       in stb state'

data Tree' a = Leaf' a 
              | Node' (Tree' a) (Tree' a)
              deriving Show

tree :: Tree' Char
tree = Node' (Node' (Leaf' 'a') (Leaf' 'b'))
             (Leaf' 'c')

rlabel :: Tree' a -> Int -> (Tree' Int, Int)
rlabel (Leaf' _) n    = (Leaf' n, n + 1)
rlabel (Node' l r) n  = (Node' l' r', n'')
  where (l', n') = rlabel l n
        (r', n'') = rlabel r n'

fresh :: ST Int
fresh  = S (\n -> (n, n + 1))

alabel :: Tree' a -> ST (Tree' Int)
alabel (Leaf' _)    = Leaf' <$> fresh
alabel (Node' l r)  = Node' <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do
  n <- fresh
  return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')

conv :: Char -> Maybe Int
conv c 
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []      = return []
filterM p (x:xs)  = do
  b <- p x
  ys <- filterM p xs
  return (if b then x:ys else ys)

join :: Monad m => m (m a) -> m a
join mmx = do
  mx <- mmx
  x <- mx
  return x

-- Exercises
-- No. 1
data Tree'' a = Leaf''
              | Node'' (Tree'' a) a (Tree'' a)

instance Functor Tree'' where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f Leaf''         = Leaf'' 
  fmap f (Node'' l a r) = Node'' (fmap f l) (f a) (fmap f r)

-- No. 4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> f a -> f b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x =  Z [x]

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | g <- gs, x <- xs]
