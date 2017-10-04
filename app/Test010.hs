{-# LANGUAGE Safe #-}

module Test010 where

data FibCount = FibCount !Int !Int deriving Show

fibcount :: Int -> FibCount
fibcount 0 = FibCount 0 1
fibcount 1 = FibCount 1 1
fibcount n = FibCount (v1 + v2) (c1 + c2 + 1)
  where
    FibCount v1 c1 = fibcount (n-1)
    FibCount v2 c2 = fibcount (n-2)

test1 :: IO ()
test1 = do
  print (fibcount 42)
  print (fibcount 43)
  print (fibcount 44)
