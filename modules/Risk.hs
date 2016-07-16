{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Monoid

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atk def) =
    sequence (replicate natk die) >>= \atkRolls ->
    sequence (replicate ndef die) >>= \defRolls ->
    let result = mconcat (zipWith faceoff (hilo atkRolls) (hilo defRolls))
        newAtk = atk - (getSum . fst $ result)
        newDef = def - (getSum . snd $ result)
    in return (Battlefield newAtk newDef)
    where
        natk = max 0 $ min 3 (atk - 1)
        ndef = max 0 $ min 2 def
        hilo = reverse . sort
        faceoff aval dval = if (aval > dval) then (Sum 0, Sum 1) else (Sum 1, Sum 0)

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield atk def)
    | (atk < 2) || (def == 0) = return b
    | otherwise               = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = (/1000) . fromIntegral . length . filter atkWins <$> mapM invade (replicate 1000 b)
    where
        atkWins (Battlefield atk def) = (def == 0)
