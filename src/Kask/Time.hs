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
    , Stopwatch
    , stopwatch
    , stopwatch'
    , elapsedMsecs
    , logging
    )
    where

import Control.Exception (evaluate)
import System.Clock (Clock (Monotonic), getTime, diffTimeSpec, toNanoSecs
                    , TimeSpec)

data Stopwatch = Stopwatch Clock TimeSpec

stopwatch' :: Clock -> IO Stopwatch
stopwatch' c = fmap (Stopwatch c) (getTime c)

stopwatch :: IO Stopwatch
stopwatch = stopwatch' Monotonic

elapsedMsecs :: Stopwatch -> IO Double
elapsedMsecs (Stopwatch c start) = do
  end <- getTime c
  let nanos = toNanoSecs $ diffTimeSpec end start
  return (fromInteger nanos / 1e6)

withMsecsIO' :: Clock -> IO a -> IO (a, Double)
withMsecsIO' c action = do
  swatch <- stopwatch' c
  value  <- action
  msecs  <- elapsedMsecs swatch
  return (value, msecs)

withMsecsIO :: IO a -> IO (a, Double)
withMsecsIO = withMsecsIO' Monotonic

withMsecs' :: Clock -> a -> IO (a, Double)
withMsecs' c = withMsecsIO' c . evaluate

withMsecs :: a -> IO (a, Double)
withMsecs = withMsecs' Monotonic

logging :: String -> IO (a, Double) -> IO a
logging s action = do
  (value, msecs) <- action
  putStrLn $ s ++ show msecs ++ " msecs"
  return value
