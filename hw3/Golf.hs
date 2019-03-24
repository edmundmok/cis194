{-# OPTIONS_GHC -Wall #-}

module Golf where

skips :: [a] -> [[a]]
skips xs = [map fst (filter (\(_, y) -> (y `mod` n == 0)) (zip xs [1..]))  | n <- [1..length xs]]
