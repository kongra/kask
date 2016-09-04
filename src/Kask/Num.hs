{-# LANGUAGE              Safe #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------
-- |
-- Module      : Kask.Num
-- Copyright   : (c) 2016 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2016-09-04
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Numbers utilities
------------------------------------------------------------------------
module Kask.Num
       (
       )
       where

--   Safe
-- , Natural
-- , Positive
-- , SafeNatural
-- , toNatural
-- , fromNatural

-- TODO:
-- instance Enum Integer
-- instance Num Integer
-- instance Real Integer
-- instance Show Integer

-- newtype Natural  a = Natural  a deriving (Eq, Ord, Read) -- Enum
-- newtype Positive a = Positive a deriving (Eq, Ord, Read) -- Enum

-- type Safe n a = Either String (n a)

-- toNatural :: (Show a, Integral a) => a -> SafeNatural a
-- toNatural n
--   | n >= 0    = Right (Natural n)
--   | otherwise = Left $ "Illegal negative Natural " ++ show n
-- {-# INLINE toNatural #-}

-- fromNatural :: Natural a -> a
-- fromNatural (Natural a) = a
-- {-# INLINE fromNatural #-}

-- instance Show a => Show (Natural a) where
--   show (Natural a) = show a
--   {-# INLINE show #-}

-- instance (Integral a) => Real (Natural a) where
--   toRational (Natural n) = toRational n
--   {-# INLINE toRational #-}

-- instance (Integral a) => Num (Natural a) where
--   (+) (Natural m) (Natural n) = Natural (m + n)

-- (+)
-- (*)
-- toRational

-- test1 :: Maybe (Natural Int)
-- test1 = do
--   (Natural x) <- toNatural (5 :: Int)
--   (Natural y) <- toNatural (6 :: Int)
--   toNatural (x * y)


-- class ToInt a where toInt :: a -> Int

-- instance ToInt (Natural Int) where
--   toInt (Natural n) = n

-- class ToInteger a where toInteger :: a -> Int
