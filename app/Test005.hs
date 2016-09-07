{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash   #-}

module Test005 where

import Kask.Time
import GHC.Prim
import GHC.Exts

fib1 :: Integer -> Integer
fib1 n
  | n == 0 || n == 1 = n
  | otherwise = fib1 (n - 1) + fib1 (n - 2)

fib2 :: Int -> Int
fib2 n
  | n == 0 || n == 1 = n
  | otherwise = fib2 (n - 1) + fib2 (n - 2)


fib3 :: Int# -> Int#
fib3 n# = case n# of
  0# -> 0#
  1# -> 1#
  _  -> fib3 (n# -# 1#) +# fib3 (n# -# 2#)


test1 :: IO ()
test1 = do
  v1 <- logging "fib1 took " (withMsecs (fib1 40)) " msecs"
  print v1
  v2 <- logging "fib2 took " (withMsecs (fib2 40)) " msecs"
  print v2
  let s = fib3 40#
  v3 <- logging "fib3 took " (withMsecs (I# s)) " msecs"
  print v3
