{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Search.Tree
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-25
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- (Rose) tree search and traversal algorithms
------------------------------------------------------------------------
module Kask.Search.Tree
    (
      -- * Search abstraction
      Adjs
    , Goal
    , Strategy

      -- * Tree traversal mechanisms
    , breadthFirstTreeLevels
    , breadthFirstTreeList

      -- * Tree search
    , treeSearch
    , breadthFirst
    , depthFirst
    )
    where

-- | The type of functions that generate children of a given (rose)
-- tree node.
type Adjs a = a -> [a]

-- | Goal of the tree search process.
type Goal a = a -> Bool

-- | A tree search strategy.
type Strategy a = [a]  -- ^ states
               -> [a]  -- ^ children
               -> [a]  -- ^ updated states

-- | Returns a list of levels in a tree traversed breadth-first.
breadthFirstTreeLevels :: a -> Adjs a -> [[a]]
breadthFirstTreeLevels state adjs =
  takeWhile (not . null) (iterate (concatMap adjs) [state])

-- | Returns a list of tree nodes traversed breadth-first.
breadthFirstTreeList :: a -> Adjs a -> [a]
breadthFirstTreeList state = concat . breadthFirstTreeLevels state

-- | A generic, strategy-agnostic tree search algorithm.
treeSearch :: Strategy a -> Adjs a -> Goal a -> [a] -> Maybe a
treeSearch _        _    _    [] = Nothing
treeSearch strategy adjs goal (x:xs)
  | goal x    = Just x
  | otherwise = treeSearch strategy adjs goal (strategy xs (adjs x))

-- | Breadth-first search strategy.
breadthFirst :: Strategy a
breadthFirst = (++)
{-# INLINE breadthFirst #-}

-- | Depth-first search strategy.
depthFirst :: Strategy a
depthFirst  = flip (++)
{-# INLINE depthFirst #-}
