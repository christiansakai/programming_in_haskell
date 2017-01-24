-- Code
-- 2.5 Haskell scripts
double :: (Num a) => a -> a
double x = x + x

quadruple :: (Num a) => a -> a
quadruple x = double (double x)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

average :: [Int] -> Int
average ns = sum ns `div` length ns

-- Exercise
-- No. 3
n = a `div` length xs
    where
      a  = 10
      xs = [1, 2, 3, 4, 5]

-- No. 4
last' :: [a] -> a
last' xs = head (reverse xs)

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1)

-- No. 5
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

init'' :: [a] -> [a]
init'' xs = reverse . tail . reverse $ xs
