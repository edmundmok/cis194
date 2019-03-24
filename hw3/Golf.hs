{-# OPTIONS_GHC -Wall #-}

module Golf where

skips :: [a] -> [[a]]
skips xs = skipsHelper xs 1

skipsHelper :: [a] -> Int -> [[a]]
skipsHelper xs n
  | n > length xs = []
  | otherwise = map fst (filter (\(_, y) -> (y `mod` n == 0)) (zip xs [1..])) : skipsHelper xs (n+1)