------------------------------------------------------------------------
-- |
-- Module      : Kask.Print
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-09-03
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Imperative printing abstraction with IO and Text/String
-- implementations
------------------------------------------------------------------------
module Kask.Print
       ( Printable
       , printLn
       , print

       , TextBuilder
       , toText

       , LazyTextBuilder
       , toLazyText
       , toLazyTextBuilder

       , StringBuilder
       , toShowS
       , toString

       , evalShowS

       , StrCat
       , strCat
       )
       where

import qualified Control.Monad.State.Strict as S
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Data.Text.Lazy.IO          as TLIO
import           Prelude                    hiding (print)

-- ABSTRACTION
class Monad m => Printable m p where
  print   :: p -> m ()
  printLn :: p -> m ()

-- IO
instance Printable IO String where
  print   = putStr
  printLn = putStrLn
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable IO ShowS where
  print   = print    . evalShowS
  printLn = putStrLn . evalShowS
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable IO T.Text where
  print   = TIO.putStr
  printLn = TIO.putStrLn
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable IO TL.Text where
  print   = TLIO.putStr
  printLn = TLIO.putStrLn
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable IO TLB.Builder where
  print   = print   . TLB.toLazyText
  printLn = printLn . TLB.toLazyText
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

-- TEXT BUILDER
type TextBuilder = S.State T.Text

toText :: TextBuilder () -> T.Text
toText tb = snd (S.runState tb "")
{-# INLINE toText #-}

instance Printable TextBuilder String where
  print   = print   . T.pack
  printLn = printLn . T.pack
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable TextBuilder ShowS where
  print   = print   . evalShowS
  printLn = printLn . evalShowS
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable TextBuilder T.Text where
  print txt = do
    buf <- S.get
    S.put (T.append buf txt)
  {-# INLINE print #-}

  printLn txt = do
    buf <- S.get
    S.put (T.append (T.append buf txt) "\n")
  {-# INLINE printLn #-}

instance Printable TextBuilder TL.Text where
  print   = print   . TL.toStrict
  printLn = printLn . TL.toStrict
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable TextBuilder TLB.Builder where
  print   = print   . TLB.toLazyText
  printLn = printLn . TLB.toLazyText
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

-- LAZY TEXT BUILDER
type LazyTextBuilder = S.State TLB.Builder

toLazyTextBuilder :: LazyTextBuilder () -> TLB.Builder
toLazyTextBuilder tb = snd $ S.runState tb $ TLB.fromString ""
{-# INLINE toLazyTextBuilder #-}

toLazyText :: LazyTextBuilder () -> TL.Text
toLazyText = TLB.toLazyText . toLazyTextBuilder
{-# INLINE toLazyText #-}

instance Printable LazyTextBuilder String where
  print   = print   . T.pack
  printLn = printLn . T.pack
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable LazyTextBuilder ShowS where
  print   = print   . evalShowS
  printLn = printLn . evalShowS
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable LazyTextBuilder T.Text where
  print   = print   . TLB.fromText
  printLn = printLn . TLB.fromText
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable LazyTextBuilder TL.Text where
  print   = print   . TLB.fromLazyText
  printLn = printLn . TLB.fromLazyText
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable LazyTextBuilder TLB.Builder where
  print b = do
    builder <- S.get
    S.put (builder <> b)
  {-# INLINE print #-}

  printLn b = do
    builder <- S.get
    S.put (builder <> b <> TLB.fromLazyText "\n")
  {-# INLINE printLn #-}

-- STRING BUILDER
type StringBuilder = S.State ShowS

evalShowS :: ShowS -> String
evalShowS s = s ""
{-# INLINE evalShowS #-}

toShowS :: StringBuilder () -> ShowS
toShowS tb = snd (S.runState tb (showString ""))
{-# INLINE toShowS #-}

toString :: StringBuilder () -> String
toString = evalShowS . toShowS
{-# INLINE toString #-}

instance Printable StringBuilder String where
  print   = print   . showString
  printLn = printLn . showString
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable StringBuilder ShowS where
  print s = do
    buf <- S.get
    S.put (buf . s)
  {-# INLINE print #-}

  printLn s = do
    buf <- S.get
    S.put (buf . s . showString "\n")
  {-# INLINE printLn #-}

instance Printable StringBuilder T.Text where
  print   = print   . T.unpack
  printLn = printLn . T.unpack
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable StringBuilder TL.Text where
  print   = print   . TL.toStrict
  printLn = printLn . TL.toStrict
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

instance Printable StringBuilder TLB.Builder where
  print   = print   . TLB.toLazyText
  printLn = printLn . TLB.toLazyText
  {-# INLINE print   #-}
  {-# INLINE printLn #-}

-- CONCATENATION OF TEXTUAL DATA IS PRINTING INTO BUILDERS
class StrCat c where
  strCat :: (Foldable t) => t c -> c

instance StrCat String where
  strCat = strCatWith toString
  {-# INLINE strCat #-}

instance StrCat ShowS where
  strCat = strCatWith toShowS
  {-# INLINE strCat #-}

instance StrCat T.Text where
  strCat = strCatWith toText
  {-# INLINE strCat #-}

instance StrCat TL.Text where
  strCat = strCatWith toLazyText
  {-# INLINE strCat #-}

instance StrCat TLB.Builder where
  strCat = strCatWith toLazyTextBuilder
  {-# INLINE strCat #-}

strCatWith :: (Printable m c, Foldable t) => (m () -> c) -> t c -> c
strCatWith f = f . mapM_ print
{-# INLINE strCatWith #-}
