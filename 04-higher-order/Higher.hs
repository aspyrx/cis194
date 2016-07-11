module Higher where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node h _ _ _) = h

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node h l y r)
  | lh < rh = Node h il y r
  | lh > rh = Node h l y ir
  | ilh < irh = Node h il y r
  | otherwise = Node (irh + 1) l y ir
  where lh = heightTree l
        rh = heightTree r
        il = insertTree x l
        ir = insertTree x r
        ilh = heightTree il
        irh = heightTree ir

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf


xor :: [Bool] -> Bool
xor = foldr (\x y -> x /= y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\y ys -> f ys y) base xs

