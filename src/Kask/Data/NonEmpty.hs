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
       , HashMap
       , fromHashMap
       , toHashMap
       , HashSet
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

newtype HashSet a = HashSet (Set.HashSet a) deriving (Eq)

fromHashSet :: Set.HashSet a-> Maybe (HashSet a)
fromHashSet s
  | Set.null s = Nothing
  | otherwise  = Just (HashSet s)
{-# INLINE fromHashSet #-}

toHashSet :: HashSet a -> Set.HashSet a
toHashSet (HashSet m) = m
{-# INLINE toHashSet #-}

instance Show a => Show (HashSet a) where
  show (HashSet s) = show s
  {-# INLINE show #-}

-- NON-EMPTY MAP

newtype HashMap k v = HashMap (Map.HashMap k v) deriving (Eq)

fromHashMap :: Map.HashMap k v -> Maybe (HashMap k v)
fromHashMap m
  | Map.null m = Nothing
  | otherwise  = Just (HashMap m)
{-# INLINE fromHashMap #-}

toHashMap :: HashMap k v -> Map.HashMap k v
toHashMap (HashMap m) = m
{-# INLINE toHashMap #-}

instance (Show k, Show v) => Show (HashMap k v) where
  show (HashMap m) = show m
  {-# INLINE show #-}
