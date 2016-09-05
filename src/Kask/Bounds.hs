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
       , Bounds
       , minBound
       , maxBound
       , toUnbounded
       , toBounded
       , Natural (Natural)
       , Positive (Positive)
       )
       where

import qualified Kask
import Prelude hiding (Bounded, minBound, maxBound)

newtype Bounded b a = Bounded { toUnbounded :: a } deriving Show;

class (Show b, Show a, Ord a) => Bounds b a where
  minBound :: b -> a -> Maybe a
  maxBound :: b -> a -> Maybe a

checkMinBound :: (Bounds b a) => b -> a -> Kask.R a
checkMinBound bounds x = case minBound bounds x of
  Just y  -> if x >= y then Right x else Kask.error $ "TOO LOW " ++
                                         show bounds ++ " VALUE " ++
                                         show x
  Nothing -> Right x
{-# INLINE checkMinBound #-}

checkMaxBound :: (Bounds b a) => b -> a -> Kask.R a
checkMaxBound bounds x = case maxBound bounds x of
  Just y  -> if x <= y then Right x else Kask.error $ "TOO HIGH " ++
                                         show bounds ++ " VALUE " ++
                                         show x
  Nothing -> Right x
{-# INLINE checkMaxBound #-}

toBounded :: (Bounds b a) => b -> a -> Kask.R (Bounded b a)
toBounded bounds x =
  fmap Bounded (checkMinBound bounds x >>= checkMaxBound bounds)

-- NATURALS

data Natural = Natural deriving Show

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

data Positive = Positive deriving Show

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
