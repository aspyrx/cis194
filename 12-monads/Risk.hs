{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.List

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving (Show)

roll :: Int -> Rand StdGen [DieValue]
roll n = sortBy (flip compare) <$> replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield attack defend) =
    let dec n = max 0 (n - 1)
        f True (Battlefield a d) = Battlefield a (dec d)
        f False (Battlefield a d) = Battlefield (dec a) d
     in do att <- (roll $ max attack 3)
           def <- (roll $ max defend 2)
           return $ foldr f b $ zipWith (>) att def

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield attack defend) = if attack > 1 && defend > 0
                                          then do bt <- battle b
                                                  invade bt
                                          else return b

successProb :: Battlefield -> Rand StdGen Double
successProb b = let wins :: Battlefield -> Double -> Double
                    wins (Battlefield _ defend) = if defend > 0 then id else (+ 1)
                 in do bs <- replicateM 1000 (invade b)
                       return $ foldr wins 0 bs / 1000

