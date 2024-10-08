{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice i = replicateM i die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) = dice (a + d) >>= \dc -> undefined
  where
    a = min (att - 1) 3
    d = min def 2

getTroops :: Battlefield -> (Army, Army)
getTroops (Battlefield att def) = (min (att - 1) 3, min def 2)
