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
    , printMsecs
    , printMsecs'
    , logging
    , logging'
    )
    where

import RIO hiding (evaluate)
import Prelude (putStrLn)
import Control.Exception (evaluate)
import System.Clock (Clock (Monotonic), getTime, diffTimeSpec, toNanoSecs
                    , TimeSpec)

defaultClock :: Clock
defaultClock = Monotonic

data Stopwatch = Stopwatch Clock TimeSpec

stopwatch' :: Clock -> IO Stopwatch
stopwatch' c = fmap (Stopwatch c) (getTime c)

stopwatch :: IO Stopwatch
stopwatch = stopwatch' defaultClock

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
withMsecsIO = withMsecsIO' defaultClock

withMsecs' :: Clock -> a -> IO (a, Double)
withMsecs' c = withMsecsIO' c . evaluate

withMsecs :: a -> IO (a, Double)
withMsecs = withMsecs' defaultClock

printMsecs' :: (String -> IO ()) -> String -> IO Double -> String -> IO ()
printMsecs' printer prefix msecs postfix = do
  s <- msecs
  printer (prefix ++ show s ++ postfix)

printMsecs :: String -> IO Double -> String -> IO ()
printMsecs = printMsecs' putStrLn

logging' :: (String -> IO ()) -> String -> IO (a, Double) -> String -> IO a
logging' printer prefix action postfix = do
  (value, msecs) <- action
  printer $ prefix ++ show msecs ++ postfix
  return value

logging :: String -> IO (a, Double) -> String -> IO a
logging = logging' putStrLn
