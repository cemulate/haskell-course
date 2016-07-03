{-# LANGUAGE FlexibleInstances
           , OverlappingInstances #-}

-- Ex. 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Bad
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Good
fibs2 :: [Integer]
fibs2 = map snd $ iterate (\(x,y) -> (y,x+y)) (0,1)


-- Ex. 2,3,4

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show s = "Stream " ++ (show . take 20 . streamToList $ s) ++ " ..."

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamSeed :: (a -> a) -> a -> Stream a
streamSeed f x = Stream x (streamSeed f (f x))


-- Ex. 5

nats :: Stream Integer
nats = streamSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) (Stream y ys) = Stream x (Stream y (interleaveStreams xs ys))

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap calcRuler $ streamSeed (+2) 2)
    where
        calcRuler n = fromIntegral . length . takeWhile even $ iterate (`div` 2) n


-- Extra (Generating functions)

type GFunc = Stream Integer

instance Show GFunc where
    show s = polyTerm False 0 (take 10 . streamToList $ s)
        where
            polyTerm _ _ []     = " ..."
            polyTerm hit n (0:xs) = polyTerm hit (n+1) xs
            polyTerm hit n (x:xs)
                | n == 0    = (if hit then " + " else "") ++ (show x) ++ polyTerm True (n+1) xs
                | n == 1    = (if hit then " + " else "") ++ (show x) ++ "x" ++ polyTerm True (n+1) xs
                | otherwise = (if hit then " + " else "") ++ (show x) ++ "x^" ++ (show n) ++ polyTerm True (n+1) xs

x :: GFunc
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num GFunc where
    fromInteger n = Stream n (streamRepeat 0)
    negate (Stream g gs) = Stream (-g) (negate gs)
    (Stream a as) + (Stream b bs) = Stream (a+b) (as + bs)
    (Stream a0 as) * b@(Stream b0 bs) = Stream (a0*b0) ((streamMap (*a0) bs)  + as*b)

instance Fractional GFunc where
    a@(Stream a0 as) / b@(Stream b0 bs) = Stream (div a0 b0) (streamMap (`div` b0) (as - (a/b)*bs))

-- This is too cool:
fibs3 :: [Integer]
fibs3 = streamToList (x / (1-x-x^2))


-- More Extra

-- | a b |
-- | c d |
data IntMatrix = IntMatrix Integer Integer Integer Integer deriving (Show, Eq)

instance Num IntMatrix where
    fromInteger n = IntMatrix n 0 0 n
    (IntMatrix a b c d) + (IntMatrix a' b' c' d') = IntMatrix (a+a') (b+b') (c+c') (d+d')
    negate (IntMatrix a b c d) = IntMatrix (-a) (-b) (-c) (-d)
    (IntMatrix a b c d) * (IntMatrix a' b' c' d') = IntMatrix (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fn where (IntMatrix _ fn _ _) = (IntMatrix 1 1 1 0)^n
