module Golf where

skips :: [a] -> [[a]]
skips l = [[k | (k, i) <- zip l $ cycle [1..n], i == n] | n <- [1..length l]]

localMaxima :: [Integer] -> [Integer]
localMaxima l = l

