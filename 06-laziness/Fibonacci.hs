{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = let (0 : rest) = fibs2
         in [0, 1] ++ zipWith (+) fibs2 rest

infixr 5 `Stream`
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

instance Show a => Show (Stream a) where
    show s = concat ["Stream ", show . take 10 . streamToList $ s, "..."]

streamRepeat :: a -> Stream a
streamRepeat a = a `Stream` streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = f a `Stream` streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = s `Stream` streamFromSeed f (f s)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a m) r =
    let (Stream b n) = r
     in a `Stream` b `Stream` interleaveStreams m n

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler)

x :: Stream Integer
x = 0 `Stream` 1 `Stream` streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = n `Stream` streamRepeat 0
    negate = streamMap (* (-1))
    Stream a m + Stream b n = a + b `Stream` m + n
    Stream a m * r@(Stream b n) = a * b `Stream` streamMap (* a) n + m * r

instance Fractional (Stream Integer) where
    l@(Stream a m) / r@(Stream b n) = a `div` b `Stream` streamMap (`div` b) (m - (l / r * n))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

data Matrix a = Matrix a a a a

instance Num a => Num (Matrix a) where
    Matrix a b c d * Matrix e f g h = Matrix (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)


fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = case Matrix 1 1 1 0 ^ n of
            Matrix _ m _ _ -> m

