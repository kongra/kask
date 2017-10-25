{-# LANGUAGE Safe         #-}
{-# LANGUAGE BangPatterns #-}

module Test011 where

import Data.Maybe (fromJust)

type Value = Int

gen :: Value -> Maybe Value
gen n = Just (n * 3 + 1)

experiment :: Value -> IO ()
experiment n = do
  print $ "Performing experiment " ++ show n
  print $ loop 0 1 where
    -- loop :: Value -> Value -> Value
    loop s i = if i == (1000000000 + n) then s else loop (s + (fromJust (gen i))) (i + 1)

test1 :: IO ()
test1 = do
  experiment 10
  experiment 11
  experiment 12
  experiment 13
  experiment 14
  experiment 15
  experiment 16
  experiment 17
  experiment 18
  experiment 19
