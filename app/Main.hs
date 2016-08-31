{-# LANGUAGE              Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Kask.Time
import qualified Kask.Text as Text

test1 :: IO ()
test1 = do
  let coll = [1 .. 10000000] :: [Integer]
  let s    = sum coll
  value <- logging "Computations took sthing like " (withMsecs s) " msecs"
  print value

test2 :: IO ()
test2 = do
  let s = case Text.trimmed "   " of
        Just tr -> Text.show tr
        Nothing -> "-"
  print s

main :: IO ()
main = do
  test1
  test2
