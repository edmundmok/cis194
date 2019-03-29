{-# OPTIONS_GHC -Wall #-}

module Calc where

import Control.Monad
import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit v) = v
eval (Add l r) = (eval l) + (eval r)
eval (Mul l r) = (eval l) * (eval r)

evalStr :: String -> Maybe Integer
evalStr = parseExp Lit Add Mul >=> (return . eval)