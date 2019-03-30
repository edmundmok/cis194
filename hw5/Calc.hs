{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import Control.Monad
import qualified Data.Map as M
import qualified ExprT as E
import Parser
import StackVM

-- Exercise 1
eval :: E.ExprT -> Integer
eval (E.Lit v)   = v
eval (E.Add l r) = (eval l) + (eval r)
eval (E.Mul l r) = (eval l) * (eval r)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = parseExp E.Lit E.Add E.Mul >=> (return . eval)

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

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

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
  lit                       = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit                   = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 $ flip mod 7 $ add x y
  mul (Mod7 x) (Mod7 y) = Mod7 $ flip mod 7 $ mul x y

-- Exercise 5
instance Expr Program where
  lit x     = [StackVM.PushI x]
  add xs ys = xs ++ ys ++ [StackVM.Add]
  mul xs ys = xs ++ ys ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Calc.Var

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

maybeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeAdd (Just x) (Just y) = Just (x + y)
maybeAdd _ _               = Nothing

maybeMul :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeMul (Just x) (Just y) = Just (x * y)
maybeMul _ _               = Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _  = return x
  add f g m = maybeAdd (f m) (g m)
  mul f g m = maybeMul (f m) (g m)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
