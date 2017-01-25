-- Code
-- 6.1 Basic concepts
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- 6.2 Recursion on lists
product' :: Num a => [a] -> a
product' []       = 1
product' (n:ns)   = n * product' ns

length' :: [a] -> Int
length' []      = 0
length' (_:xs)  = 1 + length' xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys)
  | x <= y      = x : y : ys
  | otherwise   = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []        = []
isort (x:xs)    = insert x (isort xs)

-- 6.3 Multiple arguments
zip' :: [a] -> [b] -> [(a, b)]
zip'  []      _       = []
zip'  _       []      = []
zip'  (x:xs)  (y:ys)  = (x, y) : zip xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs      = xs
drop' _ []      = []
drop' n (_:xs)  = drop (n - 1) xs

-- 6.4 Multiple recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

qsort :: Ord a => [a] -> [a]
qsort []      = []
qsort (x:xs)  = qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]

-- 6.5 Mutual recursion
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

evens :: [a] -> [a]
evens []      = []
evens (x:xs)  = x : odds xs

odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs

init' :: [a] -> [a]
init' [_]     = []
init' (x:xs)  = x : init xs

-- Exercise
-- No. 1
factorialWithGuard n
  | n < 0     = error "Cannot have negative argument"
  | n == 0    = 1
  | otherwise = n * factorialWithGuard (n - 1)

-- No. 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- No. 4
euclid :: Int -> Int -> Int
euclid x y 
  | x == y = x
  | x < y = euclid x (y - x)
  | x > y = euclid (x - y) y

-- No. 6
and' :: [Bool] -> Bool
and' [x]    = x
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' []        = []
concat' (xs:xss)  = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 x = [x]
replicate' n x = x : replicate' (n - 1) x

(!!:) :: [a] -> Int -> a
(!!:) (x:_) 0   = x
(!!:) (x:xs) n  = (!!:) xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' x' []     = False
elem' x' (x:xs) 
  | x' == x   = True
  | otherwise = elem' x' xs

-- No. 7
merge :: Ord a => [a] -> [a] -> [a]
merge []      ys      = ys
merge xs      []      = xs
merge (x:xs)  (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- No. 8

halve :: [a] -> ([a], [a])
halve xs
  | odd (length xs) = (take ((length xs `div` 2) + 1) xs, drop (length xs - (length xs `div` 2)) xs)
  | otherwise       = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort []      = []
msort [x]     = [x]
msort x   = merge (msort first) (msort second)
            where (first, second) = halve x

-- No. 9
sum' :: Num a => [a] -> a
sum' =  foldl (+) 0

take' :: (Num a, Eq a) => a -> [b] -> [b]
take' 0 _       = []
take' n (x:xs)  = x : take' (n - 1) xs

last' :: [a] -> a
last' [x]     = x
last' (x:xs)  = last' xs
