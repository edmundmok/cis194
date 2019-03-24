{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List

skips :: [a] -> [[a]]
skips xs = [map fst (filter (\(_, y) -> (y `mod` n == 0)) (zip xs [1..])) | n <- [1..length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima xs
  | length xs < 3 = []
  | otherwise = foldr (\(x, y, z) l -> if (y > x && y > z) then (y : l) else l) [] (zip3 xs (tail xs) (tail $ tail xs))

histogram :: [Integer] -> String
histogram xs =
  let
    ys = map (\zs -> (length zs)-1) (group $ sort (filter (\x -> x >= 0 && x <= 9) xs ++ [0..9]))
  in
    unlines [concat [if (ys !! c > r) then "*" else " " | c <- [0..9]] | r <- reverse [0..(maximum ys)-1]] ++ "==========\n0123456789\n"
