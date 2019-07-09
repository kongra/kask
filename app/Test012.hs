module Test012 where

import RIO
import RIO.List (iterate)
import Prelude (print)
import Kask.Data.Tree.Search (treeSearch, breadthFirst)

evenAdjs :: Int -> Int -> [Int]
evenAdjs n i = take n (filter even (iterate (1+) (i + 1)))
{-# INLINE evenAdjs #-}

oddAdjs :: Int -> Int -> [Int]
oddAdjs n i = take n (filter odd (iterate (1+) (i + 1)))
{-# INLINE oddAdjs #-}

tescik :: Int -> Int -> Maybe Int
tescik n m = treeSearch breadthFirst adjs goal [0]
             where
               adjs i = if even i then oddAdjs n i else evenAdjs n i
               goal i = i == m
{-# INLINE tescik #-}

tescik20 :: Int -> Maybe Int
tescik20 = tescik 20

tescik200 :: Int -> Maybe Int
tescik200 = tescik 200

test1 :: IO ()
test1 = do
  print (tescik 20 79)
