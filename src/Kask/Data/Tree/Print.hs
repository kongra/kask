{-# LANGUAGE Safe                  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Data.Tree.Print
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-23
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- UNIX tree command-like representation for rose trees in
-- Haskell. Extensible, writes into monads with use of ShowS.
------------------------------------------------------------------------
module Kask.Data.Tree.Print
    (
      -- * Abstraction
      ShowM
    , showM
    , Printer
    , Merger
    , Adjs
    , Impl (..)

      -- * Implementations
    , io
    , str
    , istr

      -- * Use
    , Conf (..)
    , printTree
    )
    where

import Control.Monad (forM)
import Control.Monad.Identity (Identity)
import Data.Maybe (fromMaybe)
import Kask.Data.Function (compose, rcompose)
import Kask.Data.List (markLast)

-- | A relation between the monad in which the print is performed, the
-- printed tree nodes, and the options of the process.
class (Monad m) => ShowM m o s where
  -- | Converts the object of type s into ShowS in monad m. Uses options o.
  showM :: o -> s -> m ShowS

-- | Embraces the process of converting ShowS into a target monadic
-- type.
type Printer m a = Monad m => ShowS -> m a

-- | Merges subcomponents of the process into a target monadic type.
type Merger m a = Monad m => [a] -> m a

-- | An implementation.
data Impl m a = Impl !(Printer m a) !(Merger m a)

-- | IO Implementation. Writes to the output stream using putStr.
io :: Impl IO ()
io = Impl (putStr . evalShowS) (const $ return ())
       where evalShowS s = s ""

-- | Polymorphic implementation that prints to a resulting ShowS.
str :: Monad m => Impl m ShowS
str = Impl return (return . compose)

-- | Implementation that prints to a resulting Identity ShowS.
istr :: Impl Identity ShowS
istr = str

-- | A type of a function that takes an object s and returns its
-- children (in monad m). The argument o is options.
type Adjs m o s = Monad m => o -> s -> m [s]

-- | Configuration for the process of tree printing.
data Conf m a o s =
  Conf
  {
    impl     :: !(Impl m a )
  , adjs     :: !(Adjs m o s)
  , maxDepth :: !(Maybe Int)
  , opts     :: !o
  }

-- | Prints the tree starting with the object (root of the tree).
printTree :: (ShowM m o s) =>
        Conf m a o s
        -> s          -- ^ Object to start with (root of the tree)
        -> m a
printTree
  Conf { impl     = Impl printer merger
       , adjs     = adjs'
       , maxDepth = maxDepth'
       , opts     = opts' } s =
    printImpl printer merger adjs' opts' (mdepth' - 1) s 0 [True] True
  where
    -- When no max depth specified, we use maxBound :: Int
    mdepth  = fromMaybe maxBound maxDepth'
    mdepth' = if mdepth < 1 then 1 else mdepth
{-# INLINE printTree #-}

printImpl :: (ShowM m o s) =>
             Printer m a
          -> Merger m a
          -> Adjs m o s
          -> o
          -> Int
          -> s -> Int -> [Bool] -> Bool -> m a
printImpl printer merger adjs' opts' mdepth
          s level lastChildInfos isFirst = do
  s' <- showM opts' s
  let pfx  = if isFirst then empty else eol
      repr = if level == 0
               then compose [pfx, s']
               else compose [pfx, genIndent lastChildInfos, s']

  r <- printer repr
  rs <- if level == mdepth
          then return []  -- Do not recurse lower than maxDepth'
          else do
            let nextLevel  = level + 1
            children <- adjs' opts' s
            forM (zip children (markLast children)) $ \(child, isLast) ->
              printImpl printer merger adjs' opts' mdepth
                        child nextLevel (isLast:lastChildInfos) False
  merger (r:rs)

genIndent :: [Bool] -> ShowS
genIndent [] = error "Empty genIndent argument !!!"
genIndent (isLast:lastChildInfos) = compose [prefix, suffix]
  where
    suffix             = if isLast then forLastChild else forChild
    prefix             = rcompose (map indentSymbol (init lastChildInfos))
    indentSymbol True  = emptyIndent
    indentSymbol False = indent
{-# INLINE genIndent #-}

-- ASCII SYMBOLS

indent       :: ShowS
emptyIndent  :: ShowS
forChild     :: ShowS
forLastChild :: ShowS
eol          :: ShowS
empty        :: ShowS

indent       = showString "│   "
emptyIndent  = showString "    "
forChild     = showString "├── "
forLastChild = showString "└── "
eol          = showString "\n"
empty        = showString ""
