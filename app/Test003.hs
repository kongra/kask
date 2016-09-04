{-# LANGUAGE              Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Test003 where

import qualified Data.Text as T
import Kask.Print
import Prelude hiding (print)

test1 :: String
test1 = toString $ do
  print   ("aaa"       :: String)
  printLn $ show (127  ::    Int)
  printLn ("ąęśćółżźń" :: T.Text)

test2 :: ShowS
test2 = toShowS $ do
  print   ("bbb"       :: String)
  printLn $ show (128  ::    Int)
  printLn ("ąęśćółżźń" :: T.Text)

test3 :: T.Text
test3 = toText $ do
  print   ("ccc"       :: String)
  printLn $ show (129  ::    Int)
  printLn ("ąęśćółżźń" :: T.Text)

test4 :: IO ()
test4 = do
  print   ("ddd"       :: String)
  printLn $ show (130  ::    Int)
  printLn ("ąęśćółżźń" :: T.Text)

test5 :: IO ()
test5 = do
  printLn  test1
  printLn (test2 "")
  printLn  test3
  test4
