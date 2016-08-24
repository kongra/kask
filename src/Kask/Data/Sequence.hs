{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Data.Sequence
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-30
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Some extensions related to Data.Sequence.
------------------------------------------------------------------------
module Kask.Data.Sequence
    (
      insertBeforeFirstOccurence
    , removeFirstOccurence
    )
    where

import qualified Data.Sequence as Seq

-- | Inserts y before the first occurence of x within the
-- sequence. /O(i)/ where /i/ is the position of x in the sequence.
insertBeforeFirstOccurence :: Eq a =>
                                 a          -- ^ y - the element to insert
                              -> a          -- ^ x - the sequence member
                              -> Seq.Seq a  -- ^ the sequence
                              -> Seq.Seq a
insertBeforeFirstOccurence y x s = (prefix Seq.|> y) Seq.>< suffix
  where
    (prefix, suffix) = Seq.breakl (== x) s
{-# INLINABLE insertBeforeFirstOccurence #-}

-- | Removes the first occurence of the element from the
-- sequence. /O(i)/ where /i/ is the position of the element in the
-- sequence.
removeFirstOccurence :: Eq a =>
                           a          -- ^ the element to remove
                        -> Seq.Seq a  -- ^ the sequence
                        -> Seq.Seq a
removeFirstOccurence x s
  | Seq.null   suffix  = prefix
  | Seq.length ts == 1 = prefix
  | otherwise          = prefix Seq.>< Seq.index ts 1
  where
    (prefix, suffix) = Seq.breakl (== x) s
    ts               = Seq.tails suffix
{-# INLINABLE removeFirstOccurence #-}
