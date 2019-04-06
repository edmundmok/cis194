{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib n
  | n < 2 = n
  | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
-- Compiler magic here; fibs2 is reused.
fibs2 :: [Integer]
fibs2 = scanl (+) 0 (1:fibs2)

