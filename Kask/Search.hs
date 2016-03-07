{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Search
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-13
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Misc. search algorithms.
------------------------------------------------------------------------
module Kask.Search
    (
      treeSearch
    , breadthFirstSearch
    , depthFirstSearch
    ) where

type SearchGoal a = a -> Bool
type Adjs a = a -> [a]

type TreeSearchCombiner a = [a] -> [a] -> [a]

-- | Performs a generalized tree search as described in PAIP, Ch. 6.4
treeSearch :: TreeSearchCombiner a
           -> [a]
           -> SearchGoal a
           -> Adjs a
           -> Maybe a
treeSearch _    []     _    _    = Nothing
treeSearch comb (s:ss) goal adjs =
  if goal s
    then Just s
    else treeSearch comb states goal adjs
  where states = comb ss (adjs s)

-- | The combiner for the breadth-first search.
breadthFirstCombiner :: TreeSearchCombiner a
breadthFirstCombiner states newStates = states ++ newStates

-- | The combiner for the depth-first search
depthFirstCombiner :: TreeSearchCombiner a
depthFirstCombiner states newStates = newStates ++ states

-- | Performs a breadth-first search.
breadthFirstSearch :: [a] -> SearchGoal a -> Adjs a -> Maybe a
breadthFirstSearch = treeSearch breadthFirstCombiner

-- | Performs a depth-first search.
depthFirstSearch :: [a] -> SearchGoal a -> Adjs a -> Maybe a
depthFirstSearch = treeSearch depthFirstCombiner
