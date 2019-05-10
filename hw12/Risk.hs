{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atkSize defSize) = do
  let atk = (min 3 . flip (-) 1) atkSize
      def = min 2 defSize
  atkRolls <- replicateM atk die
  defRolls <- replicateM def die
  let sAtkRolls = (reverse . sort) atkRolls
      sDefRolls = (reverse . sort) defRolls
      sRolls = zip sAtkRolls sDefRolls
      defLosses = (foldl' (+) 0 . map (fromEnum . uncurry (>))) sRolls
      atkLosses = (length sRolls) - defLosses
  return (Battlefield (atkSize - atkLosses) (defSize - defLosses))

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield atkSize defSize)
  | atkSize < 2 || defSize  == 0 = return b
  | otherwise = (battle b) >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  results <- (replicateM 1000 . invade) b
  let successes = (foldl1' (+) . map (fromEnum . (== 0) . defenders)) results
  return $ (fromIntegral successes) / 1000
