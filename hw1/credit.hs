toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0     = []
  | otherwise  = (x `mod` 10) : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =  foldl (\ys (x, i) -> x * (2 ^ (fromInteger (i `rem` 2))): ys) [] (zip (reverse xs) [0..])

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate x = (sumDigits $ doubleEveryOther $ toDigits x) `rem` 10  == 0

