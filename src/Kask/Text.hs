{-# LANGUAGE            Safe #-}
{-# LANGUAGE PatternSynonyms #-}
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
       ( Trimmed
       , pattern Trimmed
       , trimmed
       )
       where

import Prelude hiding (null)
import Data.Text (Text, strip, null)

-- | Trimmed, non-blank Text
newtype Trimmed = MakeTrimmed Text deriving (Show, Eq);

pattern Trimmed tr <- MakeTrimmed tr

trimmed :: Text -> Maybe Trimmed
trimmed s
  | null tr   = Nothing
  | otherwise = Just (MakeTrimmed tr)
  where tr = strip s
