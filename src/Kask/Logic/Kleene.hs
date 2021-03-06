------------------------------------------------------------------------
-- |
-- Module      : Kask.Logic.Kleene
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-08-24
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Three-value Kleene logic
------------------------------------------------------------------------
module Kask.Logic.Kleene
  (
    Value (..)
  , not
  , and
  , or
  , impl
  ) where

import           Prelude hiding (False, True, and, not, or)

data Value = True | False | Unknown deriving (Show, Eq);

not :: Value -> Value
not True    = False
not False   = True
not Unknown = Unknown
{-# INLINE not #-}

and :: Value -> Value -> Value
and False       _ = False
and True        b = b
and Unknown False = False
and Unknown     _ = Unknown
{-# INLINE and #-}

or :: Value -> Value -> Value
or True       _ = True
or False      b = b
or Unknown True = True
or Unknown    _ = Unknown
{-# INLINE or #-}

-- | Logical implication a -> b = not(a) or b
impl :: Value -> Value -> Value
impl a b = not a `or` b
{-# INLINE impl #-}
