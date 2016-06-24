import Data.List

-- Ex. 1

fun1 :: [Integer] -> Integer
fun1 []         = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1          = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- Reimplement in better Haskell style:

fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate step
    where
        step x = if (even x) then (div x 2) else (3 * x + 1)

-- Ex. 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

ht :: Tree a -> Integer
ht Leaf = -1
ht (Node h _ _ _) = h

balancedInsert :: a -> Tree a -> Tree a
balancedInsert x Leaf = Node 0 Leaf x Leaf
balancedInsert x (Node height left value right)
    | (ht left) < (ht right)   = Node height newLeft value right
    | (ht left) == newLHeight  = Node height newLeft value right
    | (ht right) == newRHeight = Node height left value newRight
    | otherwise                = Node (height + 1) left value newRight
    where
        newLeft@(Node newLHeight _ _ _) = (balancedInsert x left)
        newRight@(Node newRHeight _ _ _) = (balancedInsert x right)

foldTree :: [a] -> Tree a
foldTree = foldr balancedInsert Leaf

-- Ex. 3

xor :: Bool -> Bool -> Bool
xor a b = (a && (not b)) || ((not a) && b)

lxor :: [Bool] -> Bool
lxor = foldr xor False

-- Could also do this; seems much better
lxor' :: [Bool] -> Bool
lxor' = odd . length . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f base xs = foldr (flip f) base (reverse xs)


-- Ex. 4

cartProd :: [a] -> [b] -> [(a,b)]
cartProd as bs = [(x, y) | x <- as, y <- bs]

sieve :: Integer -> [Integer]
sieve n = map (\x -> 2*x+1) . ([1..n] \\) . filter (<= n) . map (\(i,j) -> i+j+2*i*j) $ (cartProd [1..n] [1..n])
