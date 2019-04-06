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
-- Reducer acts as the prev element, scanl index acts as second last element.
fibs2 :: [Integer]
fibs2 = scanl (+) 0 (1:fibs2)

-- Exercise 3
data Stream a = Cons a (Stream a)

instance (Show a) => Show (Stream a) where
  -- shows only first 20 elements of the stream
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- I think this is efficient?
iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = scanl (\y _ -> f y) x $ iterate1 f x

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x $ Cons y $ interleaveStreams xs ys
