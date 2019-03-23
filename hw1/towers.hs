type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0 = []
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n == 0 = []
  | n == 1 = [(a, b)]
  | otherwise = hanoi4 (n-2) a d b c ++ hanoi 2 a b c ++ hanoi4 (n-2) d b a c
