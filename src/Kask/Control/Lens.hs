------------------------------------------------------------------------
-- |
-- Module      : Kask.Control.Lens
-- Copyright   : (c) 2015 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-03-09
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Lightweight lens implementation.
------------------------------------------------------------------------
module Kask.Control.Lens
  (
    Lens
  , over
  , view
  , set
  , _1
  , _2
  ) where

import           RIO hiding (Lens, over, set, view)

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

over :: Lens s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)
{-# INLINE over #-}

view :: Lens s a -> s -> a
view ln = getConst . ln Const
{-# INLINE view #-}

set :: Lens s a -> a -> s -> s
set ln = over ln . const
{-# INLINE set #-}

-- SAMPLE LENSES

_1 :: Functor f => (a -> f a) -> (a, b) -> f (a, b)
_1 f (x, y) = fmap (, y) (f x)
{-# INLINE _1 #-}

_2 :: Functor f => (a -> f b) -> (a, a) -> f (a, b)
_2 f (x, y) = fmap (x,) (f y)
{-# INLINE _2 #-}
