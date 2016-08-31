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
       ( Trimmed
       , trimmed
       , ShowText
       , show
       , ShowTrimmed
       , showTrimmed
       )
       where

import Prelude hiding (null, show)
import Data.Text (Text, strip, null)

class ShowText a where
  show :: a -> Text

-- | Trimmed, non-blank Text
newtype Trimmed = Trimmed Text deriving (Show, Eq);

instance ShowText Trimmed where
  show (Trimmed txt) = txt
  {-# INLINE show #-}

class ShowTrimmed a where
  showTrimmed :: a -> Trimmed

instance ShowTrimmed Trimmed where
  showTrimmed = id
  {-# INLINE showTrimmed #-}

trimmed :: Text -> Maybe Trimmed
trimmed s
  | null tr   = Nothing
  | otherwise = Just (Trimmed tr)
  where tr = strip s
