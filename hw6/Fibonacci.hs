{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}

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
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs
-- below won't work for ruler since it will always need to force the second argument out
-- and thus keep asking for the next evaluation of genStream (n+1).
-- whereas the above version only needs one argument at a time, which can simply be
-- retrieved from streamRepeat n easily.
-- interleaveStreams (Cons x xs) (Cons y ys) = Cons x $ Cons y $ interleaveStreams xs ys

ruler :: Stream Integer
ruler = genStream 0
  where genStream n = interleaveStreams (streamRepeat n) (genStream (n+1))

-- Exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons x xs') ys@(Cons y ys') = Cons (x * y) ((streamMap (*x) ys') + (xs' * ys))
  negate xs = streamMap negate xs
  fromInteger n = Cons n $ streamRepeat 0