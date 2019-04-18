{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JoinList where

import Sized

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
(+++) x y = Append (mappend (tag x) (tag y)) x y

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

eg = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')