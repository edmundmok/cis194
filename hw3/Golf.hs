{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List

skips :: [a] -> [[a]]
skips xs = [[x | (x, i) <- zip xs [1..], mod i n == 0] | n <- [1..length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_))
  | (y > x && y > z) = y : xs'
  | otherwise        = xs'
  where xs' = localMaxima xs
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs =
  let
    ys = map (pred . length) (group $ sort $ filter (\x -> x >= 0 && x <= 9) xs ++ [0..9])
  in
    unlines [map (\y -> if y > r then '*' else ' ') ys | r <- reverse [0..(maximum ys)-1]]
    ++ "==========\n0123456789\n"
