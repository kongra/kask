module Test001 where

import           Kask.Time
import           Prelude   (print)
import           RIO

test1 :: IO ()
test1 = do
  let coll = [1 .. 10000000] :: [Integer]
  let s    = sum coll
  value <- logging "Computations took sthing like " (withMsecs s) " msecs"
  print value
