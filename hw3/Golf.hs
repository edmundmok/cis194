{-# OPTIONS_GHC -Wall #-}

module Golf where

skips :: [a] -> [[a]]
skips xs = [map fst (filter (\(_, y) -> (y `mod` n == 0)) (zip xs [1..])) | n <- [1..length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima xs
  | length xs < 3 = []
  | otherwise =
    let
      ys = tail xs
      zs = tail ys
    in
      foldr (\(x, y, z) l -> if (y > x && y > z) then (y : l) else l) [] (zip3 xs ys zs)
