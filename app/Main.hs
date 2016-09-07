{-# LANGUAGE              Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Test001
import qualified Test002
import qualified Test003
import qualified Test004
-- import qualified Test005

main :: IO ()
main = do
  Test001.test1
  Test002.test1
  Test003.test5
  Test004.test1
  -- Test005.test1
