{-# LANGUAGE              Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Test002 where

import qualified Kask.Text as Text

test1 :: IO ()
test1 = do
  let s = case Text.trimmed "   " of
        Just tr -> Text.show tr
        Nothing -> "-"
  print s
