{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances #-}

import Data.Monoid
import JoinList
import Sized
import Scrabble
import Buffer

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b  = Append ((tag a) <> (tag b)) a b

makeSizedSingle :: a -> JoinList Size a
makeSizedSingle x = Single (Size 1) x

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ i _ | i < 0          = Nothing
indexJ _ Empty              = Nothing
indexJ _ (Single _ a)       = Just a
indexJ i (Append _ l1 l2)
    | i < lSize             = indexJ i l1
    | otherwise             = indexJ i l2
    where lSize = getSize . size . tag $ l1

dropJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
dropJ _ Empty               = Empty
dropJ 0 j                   = j
dropJ n (Single _ _)        = Empty
dropJ n (Append _ l1 l2)
    | n >= lSize            = dropJ (n-lSize) l2
    | otherwise             = (dropJ n l1) +++ l2
    where lSize = getSize . size . tag $ l1

takeJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
takeJ _ Empty               = Empty
takeJ 0 _                   = Empty
takeJ n s@(Single _ _)      = s
takeJ n (Append _ l1 l2)
    | n >= lSize            = l1 +++ takeJ (n-lSize) l2
    | otherwise             = takeJ n l1
    where lSize = getSize . size . tag $ l1

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-- instance Buffer (JoinList (Score, Size) String) where
--     toString = unlines . jlToList
