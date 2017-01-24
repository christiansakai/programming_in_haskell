-- Code
-- 4.1 New from old
even :: (Integral a) => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: (Fractional a) => a -> a
recip n = 1 / n

-- 4.2 Conditional expressions
abs :: Int -> Int
abs n = if n >= 0 then n else (-n)

signum :: Int -> Int
signum n = if n < 0
              then (-1)
              else
                if n == 0
                   then 0
                   else 1

-- 4.4 Pattern matching
not' :: Bool -> Bool
not' False = True
not' True  = False

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

-- 4.5 Lambda expressions
add :: Int -> Int -> Int
add = \x -> (\y -> x + y)

const :: a -> b -> a
const x = \_ -> x

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0..n-1]

-- Exercises
-- No. 1
halve :: [a] -> ([a], [a])
halve xs = (take l xs, drop l xs)
           where l = (length xs `div` 2)

-- No. 2
third :: [a] -> a
third xs = head . tail . tail $ xs

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_: _: x:_) = x

-- No. 3
safetail :: [a] -> [a]
safetail xs = if null xs then []
                         else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs   = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (x:xs) = xs

-- No. 4
True || True    = True
True || False   = True
False || True   = True
False || False = False

-- No. 5
True && True = if True then
                       if True then True
                               else False
                       else False

-- No. 6
True && b = if True then 
                    if b then b else False
                    else False

-- No. 7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

-- No. 8
luhnDouble :: Int -> Int
luhnDouble x
  | result > 9 = result - 9
  | otherwise  = result
  where result = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhnDouble a + b + luhnDouble c + d) `mod` 10) == 0
