{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances
           , GeneralizedNewtypeDeriving
           , TypeSynonymInstances
  #-}

module JoinList where

import Buffer
import Data.Monoid ((<>))
import Editor
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

-- Tries to build a balanced tree.
fromStringHelper :: Int -> Int -> [String] -> JoinList (Score, Size) String
fromStringHelper low high ls
  | low > high = Empty
  | low == high =
    let
      s = ls !! low
    in
      Single (scoreString s, Size 1) s
  | otherwise =
    let
      mid = (low + high) `div` 2
      l = fromStringHelper low mid ls
      r = fromStringHelper (mid+1) high ls
    in
      Append (tag l <> tag r) l r

instance Buffer (JoinList (Score, Size) String) where
  -- | Convert a buffer to a String.
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l r) = toString l ++ toString r

  -- | Create a buffer from a String.
  --fromString s = foldl1' (+++) $ map (\t -> Single (scoreString t, Size 1) t) $ lines s
  fromString s = fromStringHelper 0 (numOfLines - 1) ls
    where ls = lines s; numOfLines = length ls

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine _ _ Empty = Empty
  replaceLine i _ x | i < 0 || i >= (getSize $ size $ tag x) = x
  replaceLine _ s (Single _ _) = fromString s
  replaceLine i s (Append _ l r)
    | i < ls =
      let
        l' = replaceLine i s l
      in
        Append (tag l' <> tag r) l' r
    | otherwise =
      let
        r' = replaceLine (i-ls) s r
      in
        Append (tag l <> tag r') l r'
    where ls = getSize $ size $ tag l

  ---- | Compute the number of lines in the buffer.
  numLines Empty = 0
  numLines (Single (_, Size s) _) = s
  numLines (Append (_, Size s) _ _) = s

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value Empty = 0
  value (Single (Score s, _) _) = s
  value (Append (Score s, _) _ _) = s

jlBuffer :: JoinList (Score, Size) String
jlBuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main :: IO()
main = runEditor editor jlBuffer
