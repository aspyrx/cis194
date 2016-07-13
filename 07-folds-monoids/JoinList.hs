module JoinList where

import Data.Monoid
import Sized

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

