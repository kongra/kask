{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
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
      insertBefore
    , markLast
    ) where

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
{-# INLINABLE insertBefore #-}

-- | Takes a list [e0, e1, ..., en] and returns [False, False, ..., True] or
-- [False, False, ...] if the argument is infinite.
markLast :: [a] -> [Bool]
markLast []  = []
markLast (_:xs)
  | null xs   = [True]
  | otherwise = False : markLast xs
{-# NOINLINE markLast #-}
