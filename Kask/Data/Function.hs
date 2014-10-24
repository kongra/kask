{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
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
    ) where

import Data.List (foldl')

-- | Composes functions passed in a list. Uses foldl.
-- compose [f1, f2, ..., fn] = fn . ... . f2 . f1
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id
{-# INLINE compose #-}

-- | A strict version of compose. Uses fold'.
compose' :: [a -> a] -> a -> a
compose' = foldl' (flip (.)) id
{-# INLINE compose' #-}
