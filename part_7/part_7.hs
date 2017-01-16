import Data.Char
import Data.List

-- Code
add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f []      = []
map'' f (x:xs)  = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'':: (a -> Bool) -> [a] -> [a]
filter'' p []   = []
filter'' p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product xs

or' :: [Bool] -> Bool
or' []      = False
or' (x:xs)  = x || or' xs

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

product''  :: Num a => [a] -> a
product'' = foldr (*) 1

or'' :: [Bool] -> Bool
or'' = foldr (||) False

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []       = v
foldr' f v (x:xs)   = f x (foldr' f v xs)

length' :: [a] -> Int
length' []      = 0
length' (_:xs)  = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ n -> 1 + n) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' []      = []
reverse'' (x:xs)  = snoc x (reverse xs)

reverse''' :: [a] -> [a]
reverse''' = foldr snoc []

sum''' :: Num a => [a] -> a
sum''' = sum' 0
        where
          sum' v []     = v
          sum' v (x:xs) = sum' (v + x) xs

sum'''' :: Num a => [a] -> a
sum'''' = foldl (+) 0

product''' :: Num a => [a] -> a
product''' = foldl (*) 1

or''' :: [Bool] -> Bool
or''' = foldl (||) False

and''' :: [Bool] -> Bool
and''' = foldl (&&) True

length''' :: [a] -> Int
length''' = foldl (\n _ -> n + 1) 0

reverse'''' :: [a] -> [a]
reverse'''' = foldl (\xs x -> x:xs) []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []      = v
foldl' f v (x:xs)  = foldl f (f v x) xs

odd' :: Int -> Bool
odd' = not . even

twice'' :: (b -> b) -> b -> b
twice'' f = f . f

sumsqreven' :: [Int] -> Int
sumsqreven' = sum . map (^2) . filter even

id' :: a -> a
id' = \x -> x

compose' :: [a -> a] -> (a -> a)
compose' = foldr (.) id

-- Binary String Transmitter
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []      = []
chop8 bits    = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Voting algorithms
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
               [c]    -> c
               (c:cs) -> winner' (elim c bs)

-- Exercises
-- No. 1
listComp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listComp f p xs = [f x | x <- xs, p x]

listComp' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listComp' f p xs = map f (filter p xs)

-- No. 2
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile' p (x:xs)
  | p x       = dropWhile' p xs
  | otherwise = x : xs

-- No. 3
map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr (\x xs -> f x : xs) []

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' p = foldr (\x xs -> if p x then x:xs else xs) []

-- No. 5
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y

-- No. 6
unfold p h t x
  | p x         = []
  | otherwise   = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- chop8' :: [Bit] -> [[Bit]]
-- chop8' = unfold (

-- chop8 :: [Bit] -> [[Bit]]
-- chop8 []      = []
-- chop8 bits    = take 8 bits : chop8 (drop 8 bits)

-- map''' :: (a -> b) -> [a] -> [b]
-- map''' f = foldr (\x xs -> f x : xs) []


