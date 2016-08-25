module Main (main) where

import Kask.Time

main :: IO ()
main = do
  let coll = [1 .. 10000000] :: [Int]
  let s    = sum coll
  value <- logging "Computations took sthing like " (withMsecs s)
  print value
