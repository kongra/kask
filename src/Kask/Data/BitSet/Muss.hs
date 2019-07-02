------------------------------------------------------------------------
-- |
-- Module      : Kask.Data.BitSet.Muss
-- Copyright   : (c) 2016-present Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-09-10
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Minimalistic BitSet: Mutable, Unboxed, Static (non-resizable), Strict
------------------------------------------------------------------------
module Kask.Data.BitSet.Muss
       ( Size

         -- Delete when done
       , newArray
       , aget
       , aset
       , aclone
       , countBits
       )
       where

import RIO;
import qualified Data.Array.MArray as MA
import           Data.Word (Word64)
import qualified Kask.Constr as C

type Size = C.Constr (C.BoundsConstr C.Positive) Int

-- STORAGE

type ArrC  a m = MA.MArray a Word64 m -- Array Context/Constraints
type Array a   = (a Int Word64)

newArray :: ArrC a m => Size -> m (Array a)
newArray size = MA.newArray (0, C.unconstr size - 1) 0
{-# INLINE newArray #-}

aget :: ArrC a m => Array a -> Int -> m Word64
aget = MA.readArray
{-# INLINE aget #-}

aset :: ArrC a m => Array a -> Int -> Word64 -> m ()
aset = MA.writeArray
{-# INLINE aset #-}

aclone :: ArrC a m => Array a -> m (Array a)
aclone = MA.mapArray id
{-# INLINE aclone #-}

countBits :: ArrC a m => Array a -> m Int
countBits = undefined

-- API

-- TESTS

-- test1 :: IO (Array MAIO.IOUArray)
-- test1 = do
--   case (B.toBounded B.Positive 10) of
--     Right size -> newArray size
--     Left  err  -> error err

-- test2 :: IO ()
-- test2 = do
--   a1    <- test1
--   elems <- MA.getElems a1
--   P.printLn $ show $ elems
