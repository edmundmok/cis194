{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances
           , GeneralizedNewtypeDeriving
           , TypeSynonymInstances
  #-}

module JoinList where

import Data.Monoid ((<>))
import Sized
import Scrabble


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single x _)    = x
tag (Append x _ _)  = x

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single _ v) | i == 0 = Just v
indexJ i (Append x l r)
  | i < 0 || i >= xs  = Nothing
  | i < ls            = indexJ i l
  | otherwise         = indexJ (i-ls) r
  where xs = getSize $ size $ x; ls = getSize $ size $ tag l
indexJ _ _ = Nothing

eg :: JoinList Size Char
eg = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x | n <= 0        = x
dropJ _ Empty             = Empty
dropJ _ (Single _ _)      = Empty
dropJ n (Append x l r)
  | n >= xs   = Empty
  | n < ls    =
    let
      l' = dropJ n l
    in
      Append (tag l' <> tag r) l' r
  | otherwise =
    let
      r' = dropJ (n-ls) r
    in
      Append (tag r') Empty r'
  where xs = getSize $ size $ x; ls = getSize $ size $ tag l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0     = Empty
takeJ _ Empty          = Empty
takeJ _ x@(Single _ _) = x
takeJ n y@(Append x l r)
  | n >= xs    = y
  | n <= ls    = takeJ n l
  | otherwise  =
    let
      r' = takeJ (n-ls) r
    in
      Append (tag l <> tag r') l r'
  where xs = getSize $ size $ x; ls = getSize $ size $ tag l

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x
