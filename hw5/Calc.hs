{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import Control.Monad
import ExprT
import Parser
import StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit v) = v
eval (ExprT.Add l r) = (eval l) + (eval r)
eval (ExprT.Mul l r) = (eval l) * (eval r)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = parseExp ExprT.Lit ExprT.Add ExprT.Mul >=> (return . eval)

-- Exercise 3
-- Note: These functions work "generically" without even being casted to a specific type a.
-- If we want, we can also cast it to a specific instance.
-- Otherwise, it can remain a generic until forced to be casted into an instance.
-- This seems to be a key difference between Java "interfaces" and typeclasses; methods
-- aren't tied to a specific object / type.
-- See also: Any operator on Num a, e.g. +
-- :t 1 + 1
-- 1 + 1 :: Num a => a
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 $ flip mod 7 $ add x y
  mul (Mod7 x) (Mod7 y) = Mod7 $ flip mod 7 $ mul x y

-- Exercise 5
instance Expr Program where
  lit x = [StackVM.PushI x]
  add xs ys = xs ++ ys ++ [StackVM.Add]
  mul xs ys = xs ++ ys ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul