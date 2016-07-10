module Golf where

skips :: [a] -> [[a]]
skips l = [[k | (k, i) <- zip l $ cycle [1..n], i == n] | n <- [1..length l]]

localMaxima :: [Integer] -> [Integer]
localMaxima l@(_ : m@(_ : n)) = [b | (a, b, c) <- zip3 l m n, a < b && b > c]
localMaxima _ = []

histogram :: [Integer] -> String
histogram i =
    let n = [0 :: Integer ..9]
        h = [length [e | e <- i, e == x] | x <- n]
     in unlines $ [[if c >= r then '*' else ' ' | c <- h] | r <- reverse [1..maximum h]] ++ [take 10 $ repeat '=', concat $ map show n]

