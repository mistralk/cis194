type Peg = String
type Move = (Peg, Peg)

-- Exercise 5

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n _ _ _ | n < 1 = []
hanoi 1 from to temp  = [(from, to)]
hanoi n from to temp  = (hanoi (n-1) from temp to) ++
                        (hanoi 1 from to temp) ++
                        (hanoi (n-1) temp to from)

-- Exercise 6

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n from to temp1 temp2 | n < 3 = hanoi n from to temp1
                             | otherwise = (hanoi4 (n - (floor (sqrt (fromInteger (2 * n + 1))))) from temp1 temp2 to) ++
                                           (hanoi (floor (sqrt (fromInteger (2 * n + 1)))) from to temp2) ++
                                           (hanoi4 (n - (floor (sqrt (fromInteger (2 * n + 1))))) temp1 to from temp2)

main = do
  print (length (hanoi 19 "a" "b" "c"))
  print (length (hanoi4 15 "a" "b" "c" "d"))
  print ((hanoi4 15 "a" "b" "c" "d"))

