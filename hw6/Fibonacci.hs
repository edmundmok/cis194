{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib n
  | n < 2 = n
  | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]
