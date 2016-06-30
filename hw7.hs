{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances #-}

module Main where

import Data.Monoid
import JoinList
import Sized
import Scrabble
import Buffer
import Editor

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Given a list of a's and a function that constructs an m from a single a, make a balanced JoinList
buildBalanced :: Monoid m => (a -> m) -> [a] -> JoinList m a
buildBalanced _ [] = Empty
buildBalanced f [x] = Single (f x) x
buildBalanced f xs = buildBalanced f (take half xs) +++ buildBalanced f (drop half xs)
    where half = (length xs) `quot` 2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b  = Append ((tag a) <> (tag b)) a b

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ i _ | i < 0          = Nothing
indexJ _ Empty              = Nothing
indexJ 0 (Single _ a)       = Just a
indexJ _ (Single _ a)       = Nothing
indexJ i (Append _ l1 l2)
    | i < lSize             = indexJ i l1
    | otherwise             = indexJ (i-lSize) l2
    where lSize = getSize . size . tag $ l1

dropJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
dropJ _ Empty               = Empty
dropJ 0 j                   = j
dropJ n (Single _ _)        = Empty
dropJ n (Append _ l1 l2)
    | n == lSize            = l2
    | n > lSize             = dropJ (n-lSize) l2
    | n < lSize             = (dropJ n l1) +++ l2
    where lSize = getSize . size . tag $ l1

takeJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
takeJ _ Empty               = Empty
takeJ 0 _                   = Empty
takeJ n s@(Single _ _)      = s
takeJ n (Append _ l1 l2)
    | n == lSize            = l1
    | n > lSize             = l1 +++ takeJ (n-lSize) l2
    | n < lSize             = takeJ n l1
    where lSize = getSize . size . tag $ l1

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList
    fromString = buildBalanced (\x -> (scoreString x, (Size 1))) . lines
    line = indexJ
    replaceLine n str b = (takeJ n b) +++ Single (scoreString str, (Size 1)) str +++ (dropJ (n+1) b)
    numLines = (\(Size x) -> x) . snd . tag
    value = (\(Score x) -> x) . fst . tag

initBuf :: JoinList (Score, Size) String
initBuf = fromString $ unlines
    [ "This buffer is for notes you don't want to save, and for"
    , "evaluation of steam valve coefficients."
    , "To load a different file, type the character L followed"
    , "by the name of the file." ]

main = runEditor editor $ initBuf
