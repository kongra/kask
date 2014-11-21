{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Control.Monad
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-10
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Some utilites related to Control.Monad
------------------------------------------------------------------------
module Kask.Control.Monad
    (
      whenM
    , unlessM
    , mapMM
    , mapMM_
    , forMM_
    , toListM
    )
    where

import Control.Monad (when, unless, liftM)
import Data.Foldable (Foldable, toList)

-- | A version of when that works on m Bool rather than raw Bool.
whenM :: Monad m => m Bool -> m () -> m ()
whenM p s = p >>= flip when s
{-# INLINE whenM #-}

-- | A version of unless that works on m Bool rather than raw Bool.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = p >>= flip unless s
{-# INLINE unlessM #-}

-- | A version of mapM that works on m [a] rather than raw [a].
mapMM :: Monad m => (a -> m b) -> m [a] -> m [b]
mapMM f as = as >>= mapM f

-- | A version of mapM_ that works on m [a] rather than raw [a].
mapMM_ :: Monad m => (a -> m b) -> m [a] -> m ()
mapMM_ f as = as >>= mapM_ f
{-# INLINE mapMM_ #-}

-- | A version of forM_ that works on m [a] rather than raw [a].
forMM_ :: Monad m => m [a] -> (a -> m b) -> m ()
forMM_ = flip mapMM_
{-# INLINE forMM_ #-}

-- | A monadic version of Data.Foldable.toList.
toListM :: (Monad m, Foldable f) => m (f a) -> m [a]
toListM = liftM toList
{-# INLINE toListM #-}
