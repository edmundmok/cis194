{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit v) = v
eval (Add l r) = (eval l) + (eval r)
eval (Mul l r) = (eval l) * (eval r)
