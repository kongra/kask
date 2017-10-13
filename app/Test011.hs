{-# LANGUAGE Safe #-}

module Test011 where

import Data.Maybe (fromJust)

gen :: Int -> Maybe Int
gen n = Just (n + 1)

experiment :: Int -> IO ()
experiment n = do
  print $ "Performing experiment " ++ show n
  print $ loop 0 1 where
    loop :: Int -> Int -> Int
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
