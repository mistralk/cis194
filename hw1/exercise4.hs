-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x
  | x < 0 = []
  | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)


--Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:z)
  | isEvenNumber (length (x:y:z)) = 2*x : y : doubleEveryOther z
  | otherwise = x : 2*y : doubleEveryOther z

isEvenNumber :: Int -> Bool
isEvenNumber x
  | x `mod` 2 == 0 = True
  | otherwise = False


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [x] = x
sumDigits (x:y) = sum (toDigits x) + sumDigits y


-- Exercies 4

validate :: Integer -> Bool
validate x = sumDigits ((doubleEveryOther (toDigits x))) `mod` 10 == 0

main = do
  print (validate 4012888888881881)
  print (validate 4012888888881882)

