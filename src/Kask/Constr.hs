------------------------------------------------------------------------
-- |
-- Module      : Kask.Constr
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-09-19
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Constrained values constructors
------------------------------------------------------------------------
module Kask.Constr
  (
    -- * Abstraction
    Constraint
  , Constr
  , satisfies
  , constr
  , unconstr

    -- * Non-null (non-empty) constraints
  , NonNull (..)
  , IsNull
  , isNull

    -- * Numeric bounds
  , Bounds
  , BoundsConstr (..)
  , minBound
  , maxBound
  , Natural  (..)
  , Positive (..)
  ) where

import           Control.DeepSeq     (NFData)
import           Data.Hashable       (Hashable)
import           GHC.Generics        (Generic)

import qualified Data.ByteString
import qualified Data.HashMap.Strict
import qualified Data.HashSet
import qualified Data.IntMap.Strict
import qualified Data.IntSet
import qualified Data.Map.Strict
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text

import           Prelude             hiding (maxBound, minBound)

-- ABSTRACTION

class Constraint c a where
  satisfies :: c -> a -> Bool

newtype Constr c a = Constr { unconstr :: a } deriving (Show, Eq, Generic);

instance Hashable a => Hashable (Constr c a)
instance NFData   a => NFData   (Constr c a)

constr :: (Constraint c a) => c -> a -> Maybe (Constr c a)
constr c x
  | satisfies c x = Just (Constr x)
  | otherwise     = Nothing
{-# INLINE constr #-}

-- NOT-NULL (NON-EMPTY) CONSTRAINTS

class IsNull a where
  isNull :: a -> Bool

data NonNull = NonNull

instance (IsNull a) => Constraint NonNull a where
  satisfies _ = not . isNull
  {-# INLINE satisfies #-}

-- NOT-NULL (NON-EMPTY) INSTANCES (via IsNull)

instance IsNull [a] where
  isNull = null
  {-# INLINE isNull #-}

instance IsNull Data.ByteString.ByteString where
  isNull = Data.ByteString.null
  {-# INLINE isNull #-}

instance IsNull (Data.IntMap.Strict.IntMap a) where
  isNull = Data.IntMap.Strict.null
  {-# INLINE isNull #-}

instance IsNull Data.IntSet.IntSet where
  isNull = Data.IntSet.null
  {-# INLINE isNull #-}

instance IsNull (Data.Map.Strict.Map k v) where
  isNull = Data.Map.Strict.null
  {-# INLINE isNull #-}

instance IsNull (Data.Set.Set a) where
  isNull = Data.Set.null
  {-# INLINE isNull #-}

instance IsNull (Data.HashMap.Strict.HashMap k v) where
  isNull = Data.HashMap.Strict.null
  {-# INLINE isNull #-}

instance IsNull (Data.HashSet.HashSet a) where
  isNull = Data.HashSet.null
  {-# INLINE isNull #-}

instance IsNull Data.Text.Text where
  isNull = Data.Text.null
  {-# INLINE isNull #-}

instance IsNull (Data.Sequence.Seq a) where
  isNull = Data.Sequence.null
  {-# INLINE isNull #-}

-- NUMERIC BOUNDS

class (Ord a) => Bounds b a where
  minBound :: b -> a -> Maybe a
  maxBound :: b -> a -> Maybe a

checkMinBound :: (Bounds b a) => b -> a -> Bool
checkMinBound b x = case minBound b x of
  Just y  -> x >= y
  Nothing -> True
{-# INLINE checkMinBound #-}

checkMaxBound :: (Bounds b a) => b -> a -> Bool
checkMaxBound b x = case maxBound b x of
  Just y  -> x <= y
  Nothing -> True
{-# INLINE checkMaxBound #-}

newtype BoundsConstr b = BoundsConstr b

instance (Bounds b a) => Constraint (BoundsConstr b) a where
  satisfies (BoundsConstr b) a = checkMinBound b a && checkMaxBound b a
  {-# INLINE satisfies #-}

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
