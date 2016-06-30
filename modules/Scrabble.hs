{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where
import Data.Monoid
import Data.Char

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

scrabbleValues :: [(Char, Score)]
scrabbleValues = concat
    [ [(x, (Score 1)) | x <- "AEILNORSTU"]
    , [(x, (Score 2)) | x <- "DG"]
    , [(x, (Score 3)) | x <- "BCMP"]
    , [(x, (Score 4)) | x <- "FHVWY"]
    , [(x, (Score 5)) | x <- "K"]
    , [(x, (Score 8)) | x <- "JX"]
    , [(x, (Score 10)) | x <- "QZ"] ]

score :: Char -> Score
score x = maybe (Score 0) id (lookup (toUpper x) scrabbleValues)

scoreString :: String -> Score
scoreString xs = foldr (+) (Score 0) $ map score xs
