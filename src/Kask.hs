------------------------------------------------------------------------
-- |
-- Module      : Kask
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-09-04
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Imperative printing abstraction with IO and Text/String
-- implementations
------------------------------------------------------------------------
module Kask (R, error) where

import Prelude hiding (error)

-- | Universal result
type R a = Either String a

error :: String -> R a
error msg = Left msg
{-# INLINE error #-}
