{-# LANGUAGE  Trustworthy #-}
{-# LANGUAGE    MagicHash #-}
{-# LANGUAGE BangPatterns #-}
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
    , nthNaiveUnpackedFib
    , nthFib
    , fib
    )
    where

import Data.Word (Word64)
import GHC.Exts

nthNaiveFib :: Word64 -> Word64
nthNaiveFib 0 = 0
nthNaiveFib 1 = 1
nthNaiveFib n = nthNaiveFib (n - 1) + nthNaiveFib (n - 2)

nthNaiveUnpackedFib :: Int# -> Int#
nthNaiveUnpackedFib 0# = 0#
nthNaiveUnpackedFib 1# = 1#
nthNaiveUnpackedFib n = nthNaiveUnpackedFib (n -# 1#) +#
                        nthNaiveUnpackedFib (n -# 2#)

nthFib :: Word64 -> Integer
nthFib 0 = 0
nthFib 1 = 1
nthFib n = loop 0 1 n
  where
    loop !a !_  0  = a
    loop !a !b !n' = loop b (a + b) (n' - 1)
{-# INLINABLE nthFib #-}

-- INFINITE FIB SEQUENCE

data FibGen = FibGen !Integer !Integer

fibgen :: FibGen -> FibGen
fibgen (FibGen a b) = FibGen b (a + b)

fibfst :: FibGen -> Integer
fibfst (FibGen a _) = a

fib :: () -> [Integer]
fib _ = map fibfst $ iterate fibgen $FibGen 0 1
{-# INLINABLE fib #-}
