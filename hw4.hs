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
