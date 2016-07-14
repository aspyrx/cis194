{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ x = x
x +++ Empty = x
l +++ r = Append (tag l <> tag r) l r

sizedInt :: (Sized b) => b -> Int
sizedInt = getSize . size

tagInt :: (Sized b, Monoid b) => JoinList b a -> Int
tagInt = sizedInt . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append m l r)
  | i < 0 || sizedInt m <= i = Nothing
  | otherwise = if i < tagInt l then indexJ i l else indexJ (tagInt r - i) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n j | n < 1 = j
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r)
  | n >= tagInt l = dropJ (n - tagInt l) r
  | otherwise = dropJ n l +++ r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n < 1 = Empty
takeJ _ j@(Single _ _) = j
takeJ n (Append _ l r)
  | n <= tagInt l = takeJ n l
  | otherwise = l +++ takeJ (n - tagInt r) r

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
    toString = foldr (++) [] . jlToList
    fromString = foldr (+++) Empty . map (\l -> Single (scoreString l, Size 1) l) . lines
    line = indexJ
    replaceLine n s b
      | n < 0 || sizedInt (snd (tag b)) <= n = b
      | otherwise = takeJ n b +++ fromString s +++ dropJ (n + 1) b
    numLines = sizedInt . snd . tag
    value = getScore . fst . tag

main :: IO ()
main = let b :: JoinList (Score, Size) String
           b = fromString $
               "This buffer is for notes you don't want to save, and for\n" ++
                   "evaluation of steam valve coefficients.\n" ++
                       "To load a different file, type the character L followed\n" ++
                           "by the name of the file.\n"
         in runEditor editor b
