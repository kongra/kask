{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Time
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-08-25
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Misc. functionalities related to time and clocks
------------------------------------------------------------------------
module Kask.Time
    ( withMsecs
    , withMsecs'
    , withMsecsIO
    , withMsecsIO'
    , logging
    )
    where

import Control.Exception (evaluate)
import System.Clock (Clock (Monotonic), getTime, diffTimeSpec, toNanoSecs)

-- | Executes the action and returns its result together with the
-- execution time in msecs. Uses the specified Clock.
withMsecsIO' :: Clock -> IO a -> IO (a, Double)
withMsecsIO' c action = do
  start <- getTime  c
  value <- action
  end   <- getTime  c

  let nanos = toNanoSecs $ diffTimeSpec end start
  let msecs = fromInteger nanos / 1e6
  return (value, msecs)

withMsecsIO :: IO a -> IO (a, Double)
withMsecsIO = withMsecsIO' Monotonic

-- | Evaluates its second arg to WHNF returning the result together
-- with the evaluation time in msecs. Uses the specified Clock.
withMsecs' :: Clock -> a -> IO (a, Double)
withMsecs' c = withMsecsIO' c . evaluate

withMsecs :: a -> IO (a, Double)
withMsecs = withMsecs' Monotonic

logging :: String -> IO (a, Double) -> IO a
logging s action = do
  (value, msecs) <- action
  putStrLn $ s ++ show msecs ++ " msecs"
  return value
