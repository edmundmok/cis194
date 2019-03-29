{-# OPTIONS_GHC -Wall #-}

module Folds where

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then div x 2 else 3 * x + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertIntoTree Leaf

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree v Leaf = Node 0 Leaf v Leaf
insertIntoTree v (Node h Leaf v' r) = Node (h+1) (insertIntoTree v Leaf) v' r
insertIntoTree v (Node h l v' Leaf) = Node h l v' (insertIntoTree v Leaf)
insertIntoTree v (Node _ l@(Node h' _ _ _) v' r@(Node h'' _ _ _))
  | h' < h'' = let l'@(Node hh' _ _ _) = insertIntoTree v l
                in Node ((+1) $ max hh' h'') l' v' r
  | otherwise = let r'@(Node hh'' _ _ _) = insertIntoTree v r
                in Node ((+1) $ max h' hh'') l v' r'

xor :: [Bool] -> Bool
xor = odd . foldl' (\x y -> x + fromEnum y) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- g is a function (accumulator) that takes an a -> a' (initially id; abusing notation here)
-- g = \a -> (f a x) after first round
-- want: substitute some a such that it evaluates the a first (i.e. apply the result of (f a' x) within)
-- use function application to nest more functions within
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g a -> g (f a x)) id xs base

-- note: this foldr version still does not work on infinite lists, since foldl cannot work on infinite lists.
myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr f base xs = foldl (\g x a -> g (f x a)) id xs base

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (not . (flip elem) sieved) [1..n]
  where sieved = map (\(x, y) -> x + y + 2 * x * y) $ filter (\(x, y) -> x <= y) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
