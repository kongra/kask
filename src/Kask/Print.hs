{-# LANGUAGE                  Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE     FlexibleInstances #-}
{-# LANGUAGE     OverloadedStrings #-}
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

       , StringBuilder
       , toShowS
       , toString

       , evalShowS
       )
       where

import qualified Control.Monad.State.Strict as S
import           Data.Text as T
import qualified Data.Text.IO as TIO
import           Prelude hiding (print)

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

-- TEXT BUILDER

type TextBuilder = S.State T.Text

toText :: TextBuilder () -> Text
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
