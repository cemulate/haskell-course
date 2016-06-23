module Golf where
import Data.List

-- Task 1 --

chunk :: Int -> [a] -> [[a]]
chunk _ []   = []
chunk n list = [take n list] ++ (chunk n (drop n list))

everyNth :: Int -> [a] -> [a]
everyNth n list  = map last (filter (\x -> (length x) >= n) (chunk n list))


-- Which skips is the best style?

skips :: [a] -> [[a]]
skips list = [everyNth n list | n <- [1..(length list)]]

skips' :: [a] -> [[a]]
skips' list = map ((flip everyNth) list) [1..(length list)]

skips'' :: [a] -> [[a]]
skips'' list = reverse (skipsInner (length list) list)
    where
        skipsInner 0 _    = []
        skipsInner n list = [everyNth n list] ++ (skipsInner (n-1) list)


-- Task 2 --

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:ws) = (if (x < y && z < y) then [y] else []) ++ (localMaxima (y:z:ws))
localMaxima other      = []


-- Task 3 --

dropOne :: [a] -> [a]
dropOne []     = []
dropOne (x:xs) = xs

popAll :: [[a]] -> [[a]]
popAll = map dropOne

toRuns :: [Integer] -> [[Integer]]
toRuns = fillIn . group . sort
    where
        fillIn list = map (existingOrEmpty list) [0..9]
        existingOrEmpty list n = maybe [] id (find (\x -> (head x) == n) list)

histogram :: [Integer] -> String
histogram list = histogramInner (toRuns list) ++ "\n==========\n0123456789\n"
    where
        histogramInner list
            | noneLeft    = ""
            | otherwise   = (histogramInner (popAll list)) ++ "\n" ++ (foldr (++) "" (map starIfNonEmpty list))
            where noneLeft = all (\x -> (length x) == 0) list
                  starIfNonEmpty list = if (length list) > 0 then "*" else " "
