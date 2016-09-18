{-# LANGUAGE              Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE        RankNTypes #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Data.Tree.Print
-- Copyright   : (c) 2016-present Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-23
-- Re-designed : 2016-09-08
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- UNIX tree command-like representation for rose trees in
-- Haskell.
------------------------------------------------------------------------
module Kask.Data.Tree.Print
       ( Adjs
       , Show
       , Depth
       , printTree
       )
       where

import           Control.Monad (unless, forM_)
import           Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Kask.Constr as C
import           Kask.Data.List (markLast)
import qualified Kask.Print as P
import           Prelude hiding (Show, show)

type Adjs a t = Foldable t => a -> t a
type Show a s = Symbolic s => a -> s
type Depth    = C.Constr (C.BoundsConstr C.Positive) Int

printTree :: (P.Printable m s, Symbolic s, Foldable t) =>
             a -> Adjs a t -> Show a s -> Maybe Depth -> m()
printTree node adjacent show maxDepth =
  doPrintTree node adjacent show (case maxDepth of
                                     Just d  -> C.unconstr d - 1
                                     Nothing -> maxBound)
    0      -- initial level is 0-th
    [True] -- node has no siblings, so it is the last child of its parent ...
    True   -- .. and it is the first one

doPrintTree :: (P.Printable m s, Symbolic s, Foldable t) =>
               a -> Adjs a t -> Show a s -> Int -> Int -> [Bool] -> Bool -> m()
doPrintTree node adjacent show maxDepth level lastChildMarks isFirst = do
  let s    = show node
      pfx  = if isFirst then empty else eol
      repr = if level == 0
             then P.strCat [pfx, s]
             else P.strCat [pfx, genIndent lastChildMarks, s]

  P.print repr

  unless (level == maxDepth) $ do
    let children = toList $ adjacent node
    forM_ (zip children (markLast children)) $ \(child, isLast) ->
      doPrintTree child adjacent show maxDepth (level + 1 )
        (isLast : lastChildMarks) False

genIndent :: Symbolic s => [Bool] -> s
genIndent [] = empty -- should not happen anyway
genIndent (isLast:lastChildMarks) = P.strCat [prefix, suffix]
  where
    indentSymbol True  = emptyIndent
    indentSymbol False = indent
    suffix  = if isLast then forLastChild else forChild
    prefix  = P.strCat $ fmap indentSymbol $ reverse $ init lastChildMarks

-- ASCII SYMBOLS

class P.StrCat s => Symbolic s where
  indent       :: s
  emptyIndent  :: s
  forChild     :: s
  forLastChild :: s
  eol          :: s
  empty        :: s

instance Symbolic String where
  indent       = "│   "
  emptyIndent  = "    "
  forChild     = "├── "
  forLastChild = "└── "
  eol          = "\n"
  empty        = ""

instance Symbolic ShowS where
  indent       = showString   (indent       :: String)
  emptyIndent  = showString   (emptyIndent  :: String)
  forChild     = showString   (forChild     :: String)
  forLastChild = showString   (forLastChild :: String)
  eol          = showString   (eol          :: String)
  empty        = showString   (empty        :: String)

instance Symbolic T.Text where
  indent       = T.pack       (indent       :: String)
  emptyIndent  = T.pack       (emptyIndent  :: String)
  forChild     = T.pack       (forChild     :: String)
  forLastChild = T.pack       (forLastChild :: String)
  eol          = T.pack       (eol          :: String)
  empty        = T.pack       (empty        :: String)

instance Symbolic TL.Text where
  indent       = TL.pack      (indent       :: String)
  emptyIndent  = TL.pack      (emptyIndent  :: String)
  forChild     = TL.pack      (forChild     :: String)
  forLastChild = TL.pack      (forLastChild :: String)
  eol          = TL.pack      (eol          :: String)
  empty        = TL.pack      (empty        :: String)

instance Symbolic TLB.Builder where
  indent       = TLB.fromText (indent       :: T.Text)
  emptyIndent  = TLB.fromText (emptyIndent  :: T.Text)
  forChild     = TLB.fromText (forChild     :: T.Text)
  forLastChild = TLB.fromText (forLastChild :: T.Text)
  eol          = TLB.fromText (eol          :: T.Text)
  empty        = TLB.fromText (empty        :: T.Text)
