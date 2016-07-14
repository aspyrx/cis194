{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score c = let f = elem $ toLower c
           in case () of
                _ | f ['a', 'e', 'i', 'l', 'n', 'o', 'r', 's', 't', 'u'] -> 1
                  | f ['d', 'g'] -> 2
                  | f ['c', 'm', 'p'] -> 3
                  | f ['f', 'h', 'v', 'w', 'y'] -> 4
                  | f ['k'] -> 5
                  | f ['j', 'x'] -> 8
                  | f ['q', 'z'] -> 10
                  | otherwise -> 0

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

