{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Data.List
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-10
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- List utilities and algorithms.
------------------------------------------------------------------------
module Kask.Data.List
    (
      -- * Misc. algorithms
      insertBefore
    , markLast
    , nthDef
    , takeWhileI

      -- * Combinatorics for lists
    , powerset
    , kSublists
    , kSublistsR
    )
    where

-- | Inserts y before first occurence of x within the list. When y
-- does not occur in the list, returns the original list. Thanks to:
-- http://stackoverflow.com/questions/14774153/
--      a-function-that-inserts-element-y-before-first-occurrence-of-x
insertBefore :: Eq a =>
                a       -- ^ y - the element to insert
                -> a    -- ^ x - the list member
                -> [a]  -- ^ the list
                -> [a]
insertBefore _ _ [] = []
insertBefore y x (a:as)
  | a == x    = y:a:as
  | otherwise = a : insertBefore y x as

-- | Takes a list [e0, e1, ..., en] and returns [False, False, ..., True] or
-- [False, False, ...] if the argument is infinite.
markLast :: [a] -> [Bool]
markLast []  = []
markLast (_:xs)
  | null xs   = [True]
  | otherwise = False : markLast xs

-- | Works like !! but returns a default value if the index is
-- invalid. Stolen from: https://wiki.haskell.org/Avoiding_partial_functions.
-- There is atDef procedure in safe package, but this one does not use
-- any Eithers and Maybes, so is more effective.
nthDef :: a -> Int -> [a] -> a
nthDef d n xs
  | n < 0     = d
  | otherwise = case drop n xs of
    x:_ -> x
    []  -> d
{-# INLINABLE nthDef #-}

-- | Works like takeWhile, but includes the sentinel for which the
-- predicate is False.
takeWhileI :: (a -> Bool) -> [a] -> [a]
takeWhileI _ []     = []
takeWhileI p (x:xs) = x : if p x then takeWhileI p xs else []

-- | Returns a powerset of the argument.
powerset :: [a] -> [[a]]
powerset = foldr (\x ps -> ps ++ [x:p | p <- ps]) [[]]
{-# INLINABLE powerset #-}

-- | All possible ways to choose @k@ elements from a list, without
-- repetitions.
kSublists :: Int -> [a] -> [[a]]
kSublists 0 _      = [[]]
kSublists _ []     = []
kSublists k (x:xs) = map (x:) (kSublists (k-1) xs) ++ kSublists k xs

-- | All possible ways to choose @k@ elements from a list, /with repetitions/.
kSublistsR :: Int -> [a] -> [[a]]
kSublistsR 0 _          = [[]]
kSublistsR _ []         = []
kSublistsR k xxs@(x:xs) = map (x:) (kSublistsR (k-1) xxs) ++ kSublistsR k xs
