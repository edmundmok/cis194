{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List

skips :: [a] -> [[a]]
skips xs = [map fst (filter (\(_, y) -> (y `mod` n == 0)) (zip xs [1..])) | n <- [1..length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | (y > x && y > z) = y : xs
  | otherwise        = xs
  where xs = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs =
  let
    ys = map (pred . length) (group $ sort (filter (\x -> x >= 0 && x <= 9) xs ++ [0..9]))
  in
    unlines [concat [if (ys !! c > r) then "*" else " " | c <- [0..9]] | r <- reverse [0..(maximum ys)-1]] 
    ++ "==========\n0123456789\n"
