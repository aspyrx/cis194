type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | otherwise = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a

findK :: Integer -> Integer
findK n = n - round (sqrt (fromIntegral (2 * n + 1))) + 1

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n <= 2 = hanoi n a b c
  | otherwise = hanoi4 k a c b d ++ hanoi (n - k) a b d ++ hanoi4 k c b a d
  where k = findK n

