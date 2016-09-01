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

import Data.Hashable (Hashable, hashWithSalt)
import Data.Text (Text, strip, null)
import Prelude hiding (null)

class ShowText a where
  showText :: a -> Text

-- | Trimmed, non-blank Text
newtype Trimmed = Trimmed Text deriving Eq

instance Hashable Trimmed where
  hashWithSalt salt (Trimmed txt) = salt `hashWithSalt` txt
  {-# INLINE hashWithSalt #-}

instance Show Trimmed where
  show (Trimmed txt) = show txt
  {-# INLINE show #-}

instance ShowText Trimmed where
  showText (Trimmed txt) = txt
  {-# INLINE showText #-}

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
