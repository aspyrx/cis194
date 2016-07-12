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
streamToList (Stream x s) = x : streamToList s

instance Show a => Show (Stream a) where
    show s = concat ["Stream ", show . take 10 . streamToList $ s, "..."]

streamRepeat :: a -> Stream a
streamRepeat x = x `Stream` streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) = f x `Stream` streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = s `Stream` streamFromSeed f (f s)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x a) r =
    let (Stream y b) = r
     in x `Stream` y `Stream` interleaveStreams a b

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler)

