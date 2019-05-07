{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Tree
import Employee

import System.IO

glCons :: Employee -> GuestList -> GuestList
glCons x@(Emp { empFun = xFun }) (GL xs xsFun) = GL (x:xs) (xsFun + xFun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs xsFun) (GL ys ysFun) = GL (xs ++ ys) (xsFun + ysFun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ xFun) y@(GL _ yFun) = if xFun > yFun then x else y

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z (Node l ts) = f (foldl' (treeFold f) z ts) l

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b [] = (glCons b mempty, mempty)
nextLevel b bs = (glCons b $ foldl1' mappend $ map snd bs, foldl1' mappend $ map (uncurry moreFun) bs)

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . maxFunHelper

maxFunHelper :: Tree Employee -> (GuestList, GuestList)
maxFunHelper (Node e xs) = nextLevel e (map maxFunHelper xs)

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

getEmployees :: GuestList -> [String]
getEmployees (GL e _) = map empName e

main :: IO ()
main = do
  handle <- openFile "company.txt" ReadMode
  contents <- hGetContents handle
  let companyTree = read contents
      companyGuestList = maxFun companyTree
      companyGuestListFun = getFun companyGuestList
      companyGuestListEmployees = (sort . getEmployees) companyGuestList
  putStrLn $ "Total fun: " ++ show companyGuestListFun
  putStrLn $ unlines companyGuestListEmployees
  hClose handle
