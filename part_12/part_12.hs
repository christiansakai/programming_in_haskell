-- Code
data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

inc :: Functor f => f Int -> f Int
inc = fmap (+1)

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x * y | x <- xs, y <- ys]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n)    = Just n
eval (Div x y)  = do
  n <- eval x
  m <- eval y
  safediv n m

