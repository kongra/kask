module Main (main) where

import Kask.Time

main :: IO ()
main = do
  let coll = [1 .. 10000000] :: [Integer]
  let s    = sum coll
  value <- logging "Computations take " (withMsecs s)
  print value
