{-# LANGUAGE                  Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Bounds
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-09-04
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Bounded numeric types
------------------------------------------------------------------------
module Kask.Bounds
       ( Bounded
       , toUnbounded
       , toBounded
       , Natural (Natural)
       , Positive (Positive)
       )
       where

import Kask (R, error)
import Prelude hiding (Bounded, minBound, maxBound, error)

newtype Bounded b a = Bounded { toUnbounded :: a } deriving Show;

class Ord a => Bounds b a where
  minBound :: b -> a -> Maybe a
  maxBound :: b -> a -> Maybe a

checkMinBound :: (Bounds b a) => b -> a -> R a
checkMinBound bounds x = case minBound bounds x of
  Just y  -> if x >= y then Right x else error "UNDERFLOW"
  Nothing -> Right x

checkMaxBound :: (Bounds b a) => b -> a -> R a
checkMaxBound bounds x = case maxBound bounds x of
  Just y  -> if x <= y then Right x else error "OVERFLOW"
  Nothing -> Right x

toBounded :: (Bounds b a) => b -> a -> R (Bounded b a)
toBounded bounds x =
  fmap Bounded (checkMinBound bounds x >>= checkMaxBound bounds)

-- NATURALS

data Natural = Natural

instance Bounds Natural Int where
  minBound _ _ = Just 0
  maxBound _ _ = Nothing
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}

instance Bounds Natural Integer where
  minBound _ _ = Just 0
  maxBound _ _ = Nothing
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}

-- POSITIVE INTEGRALS

data Positive = Positive

instance Bounds Positive Int where
  minBound _ _ = Just 1
  maxBound _ _ = Nothing
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}

instance Bounds Positive Integer where
  minBound _ _ = Just 1
  maxBound _ _ = Nothing
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}
