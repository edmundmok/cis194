{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons x@(Emp { empFun = xFun }) (GL xs xsFun) = GL (x:xs) (xsFun + xFun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs xsFun) (GL ys ysFun) = GL (xs ++ ys) (xsFun + ysFun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ xFun) y@(GL _ yFun) = if xFun > yFun then x else y

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z (Node l ts) = f (foldl' (treeFold f) z ts) l