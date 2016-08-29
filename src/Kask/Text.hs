{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Text
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-08-29
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Misc. functionalities related to Text; constructors, validators, etc.
------------------------------------------------------------------------
module Kask.Text
       ( Trimmed (trimmedText)
       , trimmed
       )
       where

import Prelude hiding (null)
import Data.Text (Text, strip, null)

-- | Trimmed, non-blank Text
newtype Trimmed = Trimmed { trimmedText :: Text } deriving (Show, Eq);

trimmed :: Text -> Maybe Trimmed
trimmed s
  | null tr   = Nothing
  | otherwise = Just (Trimmed tr)
  where tr = strip s
