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
       ( NonBlank
       , Stripped
       , StrippedNonBlank
       , ShowText
       , strip
       , stripNonBlank
       , showText
       )
       where

import RIO
import Control.DeepSeq (NFData  )
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic )

import qualified Data.Text   as T
import qualified Kask.Constr as C

class ShowText a where
  showText :: a -> T.Text

-- | Stripped text
newtype Stripped = Stripped T.Text deriving (Eq, Generic)

instance Hashable Stripped
instance NFData   Stripped

instance Show Stripped where
  show (Stripped txt) = show txt
  {-# INLINE show #-}

instance ShowText Stripped where
  showText (Stripped txt) = txt
  {-# INLINE showText #-}

class Strip a where
  strip :: a -> Stripped

instance Strip T.Text where
  strip = Stripped . T.strip
  {-# INLINE strip #-}

instance Strip String where
  strip = strip . T.pack
  {-# INLINE strip #-}

-- | NonNull constraint for Stripped

instance C.IsNull Stripped where
  isNull (Stripped txt) = C.isNull txt
  {-# INLINE isNull #-}

-- | Non-blank textual content

type NonBlank         = C.Constr C.NonNull T.Text
type StrippedNonBlank = C.Constr C.NonNull Stripped

stripNonBlank :: Strip a => a -> Maybe StrippedNonBlank
stripNonBlank = C.constr C.NonNull . strip
{-# INLINE stripNonBlank #-}
