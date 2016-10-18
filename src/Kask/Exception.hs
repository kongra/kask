{-# LANGUAGE             Safe #-}
{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Exception
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-10-18
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Exception/Error-related stuff
------------------------------------------------------------------------
module Kask.Exception
    ( tryAny
    , tryAnyDeep
    , catchAny
    , catchAnyDeep
    )
    where

import Control.Concurrent.Async (withAsync,    waitCatch)
import Control.DeepSeq          (NFData,           ($!!))
import Control.Exception        (SomeException, evaluate)

-- CATCHING ANY EXCEPTION, SEE:
-- https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/
--                                catching-all-exceptions

tryAny :: IO a -> IO (Either SomeException a)
tryAny action = withAsync action waitCatch
{-# INLINE tryAny #-}

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny action onE = tryAny action >>= either onE return
{-# INLINE catchAny #-}

tryAnyDeep :: NFData a => IO a -> IO (Either SomeException a)
tryAnyDeep action = tryAny $ do
  res <- action
  evaluate $!! res -- here's the magic
{-# INLINE tryAnyDeep #-}

catchAnyDeep :: NFData a => IO a -> (SomeException -> IO a) -> IO a
catchAnyDeep action onE = tryAnyDeep action >>= either onE return
{-# INLINE catchAnyDeep #-}
