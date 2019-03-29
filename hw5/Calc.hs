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

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = parseExp Lit Add Mul >=> (return . eval)

-- Exercise 3
-- Note: These functions work "generically" without even being casted to a specific type a.
-- If we want, we can also cast it to a specific instance.
-- See also: Any operator on Num a, e.g. +
-- :t 1 + 1
-- 1 + 1 :: Num a => a
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul