toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0     = []
  | otherwise  = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = foldl (\ys (i, x) -> x * (if odd i then 2 else 1) : ys) [] (zip [0..] (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate = (==) 0 . flip rem 10 . (sumDigits . doubleEveryOther . toDigits)