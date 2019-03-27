{-# OPTIONS_GHC -Wall #-}

module Folds where

fun1 :: [Integer] -> Integer
fun1 = product . map ((-) 2) . filter even
