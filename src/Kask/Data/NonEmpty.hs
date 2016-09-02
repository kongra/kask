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
-- Minimalistic non-empty containers API
------------------------------------------------------------------------
module Kask.Data.NonEmpty
       ( List
       , fromList
       , toList
       , Map
       , fromHashMap
       , toHashMap
       , Set
       , fromHashSet
       , toHashSet
       )
       where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- NON-EMPTY LIST

newtype List a = List [a] deriving (Eq)

fromList :: [a] -> Maybe (List a)
fromList [] = Nothing
fromList l  = Just (List l)
{-# INLINE fromList #-}

toList :: List a -> [a]
toList (List l) = l
{-# INLINE toList #-}

instance Show a => Show (List a) where
  show (List l) = show l
  {-# INLINE show #-}

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

instance Show a => Show (Set a) where
  show (Set s) = show s
  {-# INLINE show #-}

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

instance (Show k, Show v) => Show (Map k v) where
  show (Map m) = show m
  {-# INLINE show #-}
