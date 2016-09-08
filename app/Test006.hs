{-# LANGUAGE              Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Test006 where

import qualified Data.Text as T
import Kask.Print
import Prelude hiding (print)

test1 :: String
test1 = strCat ["aaa", "bbb", "ccc"]

test2 :: ShowS
test2 = strCat [showString "aaa", showString "bbb", showString "ccc"]

test3 :: T.Text
test3 = strCat ["aaa", "bbb", "ccc"]

test :: IO ()
test = do
  printLn test1
  printLn test2
  printLn test3
