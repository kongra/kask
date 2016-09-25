{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Math
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-09-25
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
------------------------------------------------------------------------
module Kask.Math
    ( nthNaiveFib
    , nthFib
    )
    where

import Data.Word (Word64)

nthNaiveFib :: Word64 -> Word64
nthNaiveFib 0 = 0
nthNaiveFib 1 = 1
nthNaiveFib n = nthNaiveFib (n - 1) + nthNaiveFib (n - 2)
{-# NOINLINE nthNaiveFib #-}

nthFib :: Word64 -> Integer
nthFib 0 = 0
nthFib 1 = 1
nthFib n = loop 0 1 n
  where
    loop a _ 0  = a
    loop a b n' = loop b (a + b) (n' - 1)
{-# INLINABLE nthFib #-}
