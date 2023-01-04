module Main (main) where

import           Control.Monad (join)
import           Data.Monoid
import           Prelude       (fmap, iterate, print, pure)
import           RIO

newtype Collatz a = Collatz a

collatzTrans :: Integral a => Collatz a -> Collatz a
collatzTrans (Collatz n) = Collatz $ if even n then n `div` 2 else 3 * n + 1

notOne :: Integral a => Collatz a -> Bool
notOne (Collatz n) = n /= 1

collatzSeq :: Integral a => Collatz a -> [Collatz a]
collatzSeq = takeWhile notOne . iterate collatzTrans

collatzLen :: Integral a => Collatz a -> Int
collatzLen = length . collatzSeq

euler14 :: Integral a => a -> (a, Int)
euler14 n = loop 0 0 1 where
  loop maxLen j i =
    if i == n then
      (j, maxLen)
    else
      let len = collatzLen (Collatz i) in
        if len > maxLen then
          loop len i (i+1)
        else
          loop maxLen j (i+1)

type Euler14Result = (Int, Int)

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
  (==) Mon  Mon  = True
  (==) Tue  Tue  = True
  (==) Weds Weds = True
  (==) Thu  Thu  = True
  (==) Fri  Fri  = True
  (==) Sat  Sat  = True
  (==) Sun  Sun  = True
  (==) _   _     = False

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) = optOp

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

optOp :: Semigroup a => Optional a -> Optional a -> Optional a
optOp  Nada     Nada     = Nada
optOp  Nada    (Only a') = Only a'
optOp (Only a)  Nada     = Only a
optOp (Only a) (Only a') = Only (a <> a')

newtype Email     = Email     String deriving (Show)
newtype FirstName = FirstName String deriving (Show)

data Profile = Profile Email FirstName deriving (Show)

makeEmail :: String -> Maybe Email
makeEmail = Just . Email

makeFirstName :: String -> Maybe FirstName
makeFirstName = Just . FirstName

makeProfile :: String -> String -> Maybe Profile
makeProfile email firstName =
  Profile <$> makeEmail email <*> makeFirstName firstName

mkProfile :: String -> String -> Maybe Profile
mkProfile email firstName = do
  email'     <- makeEmail     email
  firstName' <- makeFirstName firstName
  return (Profile email' firstName')

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

main :: IO ()
main = do
  print (euler14 (1000000 + 0) :: Euler14Result)
  print (euler14 (1000000 + 1) :: Euler14Result)
  print (euler14 (1000000 + 2) :: Euler14Result)
  print (euler14 (1000000 + 3) :: Euler14Result)
  print (euler14 (1000000 + 4) :: Euler14Result)
  print (euler14 (1000000 + 5) :: Euler14Result)
  print (euler14 (1000000 + 6) :: Euler14Result)
  print (euler14 (1000000 + 7) :: Euler14Result)
  print (euler14 (1000000 + 8) :: Euler14Result)
  print (euler14 (1000000 + 9) :: Euler14Result)

-- import Criterion.Main
-- import qualified Test002
-- import qualified Test003
-- import qualified Test004
-- import qualified Test005
-- import qualified Test006
-- import qualified Test007
-- import qualified Test008
-- import qualified Test009
-- import qualified Test010
-- import qualified Test011
-- import qualified Test012
-- import qualified Lexicon

-- main =
  -- Test012.test1
  -- Test001.test1
  -- Test002.test1
  -- Test003.test5
  -- Test004.test1
  -- Test005.test1
  -- Test006.test
  -- Test007.test31
  -- Test007.test32
  -- Test008.test1
  -- Test008.test2
  -- Test009.test1
  -- Test010.test1
  -- Lexicon.test1

-- main = defaultMain [
--   bgroup "fib" [ bench "100"    $ whnf Test012.tescik100 100
--                , bench "100000" $ whnf Test012.tescik100 1000
--              --, bench "79"     $ whnf Test012.tescik20 79
--             -- , bench "430"    $ whnf Test012.tescik200 430
--                ]]
