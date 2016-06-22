toDigitsRev    :: Integer -> [Integer]
toDigitsRev n
    | n < 1     = []
    | n < 10    = [n]
    | otherwise = (mod n 10) : (toDigitsRev (div n 10))

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther [x]        = [x]
doubleEveryOther (x:(y:zs)) = [x, 2*y] ++ (doubleEveryOther zs)

doubleEveryOtherRight :: [Integer] -> [Integer]
doubleEveryOtherRight = reverse . doubleEveryOther . reverse

allToDigits :: [Integer] -> [Integer]
allToDigits []     = []
allToDigits (x:ys) = (toDigits x) ++ (allToDigits ys)

validate :: Integer -> Bool
validate n = ((sum . allToDigits . doubleEveryOtherRight . toDigits) n) `mod` 10 == 0


type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3
    | n < 1     = []
    | otherwise = (hanoi (n-1) p1 p3 p2) ++ [(p1, p2)] ++ (hanoi (n-1) p3 p2 p1)
