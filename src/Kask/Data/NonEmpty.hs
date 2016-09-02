{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Data.NonEmpty
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-24
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Minimalistic non-empty unordered containers API
------------------------------------------------------------------------
module Kask.Data.NonEmpty
       ( Map
       , fromHashMap
       , toHashMap
       , Set
       , fromHashSet
       , toHashSet
       )
       where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- NON-EMPTY SET

newtype Set a = Set (Set.HashSet a) deriving (Eq)

fromHashSet :: Set.HashSet a-> Maybe (Set a)
fromHashSet s
  | Set.null s = Nothing
  | otherwise  = Just (Set s)
{-# INLINE fromHashSet #-}

toHashSet :: Set a -> Set.HashSet a
toHashSet (Set m) = m
{-# INLINE toHashSet #-}

-- NON-EMPTY MAP

newtype Map k v = Map (Map.HashMap k v) deriving (Eq)

fromHashMap :: Map.HashMap k v -> Maybe (Map k v)
fromHashMap m
  | Map.null m = Nothing
  | otherwise  = Just (Map m)
{-# INLINE fromHashMap #-}

toHashMap :: Map k v -> Map.HashMap k v
toHashMap (Map m) = m
{-# INLINE toHashMap #-}
