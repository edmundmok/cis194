{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | d `elem` ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U'] = 1
  | d `elem` ['D', 'G']                 = 2
  | d `elem` ['B', 'C', 'M', 'P']       = 3
  | d `elem` ['F', 'H', 'V', 'W', 'Y']  = 4
  | d `elem` ['K']                      = 5
  | d `elem` ['J', 'X']                 = 8
  | d `elem` ['Q', 'Z']                 = 10
  | otherwise                           = 0
  where d = toUpper c

scoreString :: String -> Score
scoreString = mconcat . map score
