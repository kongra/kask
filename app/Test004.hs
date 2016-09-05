{-# LANGUAGE                  Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test004 where

import qualified Kask
import qualified Kask.Bounds as B
import qualified Kask.Print as P

data Velocity = Velocity deriving Show;

instance B.Bounds Velocity Double where
  minBound _ _ = Just 0
  maxBound _ _ = Just 100

velocity :: Double -> Kask.R (B.Bounded Velocity Double)
velocity = B.toBounded Velocity

test1 :: IO ()
test1 = do
  P.printLn $ show $ velocity (-1)
  P.printLn $ show $ velocity 0.0
  P.printLn $ show $ velocity 0.2456
  P.printLn $ show $ velocity 99.99
  P.printLn $ show $ velocity 100
  P.printLn $ show $ velocity 200
