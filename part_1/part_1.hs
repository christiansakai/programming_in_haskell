-- Code
-- 1.1 Functions
double :: (Num a) => a -> a
double x = x + x

-- 1.5 A Taste of Haskell
sum' :: (Num a) => [a] -> a
sum' [x]        = x
sum' (x:xs)     = x + sum' xs

qsort' :: (Ord a) => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]

seqn' :: [IO a] -> IO [a]
seqn' []          = return []
seqn' (act:acts)  = do
  x <- act
  xs <- seqn' acts
  return (x: xs)

-- Exercise
-- No. 1
quadruple :: (Num a) => a -> a
quadruple x = double (double x) 

-- No. 3
product' :: (Num a) => [a] -> a
product' [x]     = x
product' (x:xs)  = x * product' xs

-- No. 4
qsortReserve :: (Ord a) => [a] -> [a]
qsortReserve []     = []
qsortReserve (x:xs) = qsortReserve larger ++ [x] ++ qsortReserve smaller
                      where
                        smaller = [a | a <- xs, a <= x]
                        larger  = [b | b <- xs, b > x]
