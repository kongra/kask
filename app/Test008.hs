{-# LANGUAGE MagicHash #-}
module Test008 where

import RIO
import Prelude (print)
import GHC.Exts

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

test1 :: IO ()
test1 = do
  print $ fib 45
  print $ fib 46
  print $ fib 47

foo :: Int -> Int
foo 0 = 0
foo n = foo (n - 1)

test2 :: IO ()
test2 = print (foo 50000000000)

goo :: Int# -> Int#
goo 0# = 0#
goo n = goo (n -# 1#)

test3 :: IO ()
test3 = print (I# (goo 50000000000#))
