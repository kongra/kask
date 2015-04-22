{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Data.List
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-24
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Function-related utilities and algorithms
------------------------------------------------------------------------
module Kask.Data.Function
    (
      compose
    , compose'
    , rcompose
    , rcompose'
    ) where

import Data.List (foldl')

-- | Composes functions passed in a list. Uses foldl.
-- compose [f1, f2, ..., fn] = f1 . f2 . ... . fn
compose :: [a -> a] -> a -> a
compose = foldl (.) id
{-# INLINE compose #-}

-- | A strict version of compose. Uses fold'.
compose' :: [a -> a] -> a -> a
compose' = foldl' (.) id
{-# INLINE compose' #-}

-- | Composes functions passed in a list in a reversed order.
-- Uses foldl. compose [f1, f2, ..., fn] = fn . ... . f2 . f1
rcompose :: [a -> a] -> a -> a
rcompose = foldl (flip (.)) id
{-# INLINE rcompose #-}

-- | A strict version of rcompose. Uses fold'.
rcompose' :: [a -> a] -> a -> a
rcompose' = foldl' (flip (.)) id
{-# INLINE rcompose' #-}
