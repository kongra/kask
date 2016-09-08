{-# LANGUAGE Safe #-}

module Test007 where

import qualified Data.Text            as T
import qualified Kask.Bounds          as B
import qualified Kask.Data.Tree.Print as PT
import qualified Kask.Print           as P

depth :: Int -> Maybe PT.Depth
depth d = case B.toBounded B.Positive d of
  Right b -> Just b
  Left  e -> error e

-- TEST 1 - SINGLE NODE TREE

data T1 = T1

adjs1 :: T1 -> [T1]
adjs1 _ = []

show11 :: T1 -> T.Text
show11 _ = T.pack "T1"

show12 :: T1 -> String
show12 _ = "T1"

show13 :: T1 -> ShowS
show13 _ = showString "T1"

test11 :: IO ()
test11 = PT.printTree T1 adjs1 show11 (depth 1)

test12 :: IO ()
test12 = PT.printTree T1 adjs1 show12 (depth 2)

test13 :: IO ()
test13 = PT.printTree T1 adjs1 show13 (depth 3)

-- TEST 2 - SIMPLE TREE

data T2 = A2 | B2 | C2

adjs2 :: T2 -> [T2]
adjs2 A2 = [B2, C2]
adjs2 B2 = []
adjs2 C2 = []

show21 :: T2 -> T.Text
show21 A2 = T.pack "A2"
show21 B2 = T.pack "B2"
show21 C2 = T.pack "C2"

show22 :: T2 -> String
show22 A2 = "A2"
show22 B2 = "B2"
show22 C2 = "C2"

show23 :: T2 -> ShowS
show23 A2 = showString "A2"
show23 B2 = showString "B2"
show23 C2 = showString "C2"

test21 :: IO ()
test21 = PT.printTree A2 adjs2 show21 (depth 2)

test22 :: IO ()
test22 = PT.printTree A2 adjs2 show22 (depth 2)

test23 :: IO ()
test23 = PT.printTree A2 adjs2 show23 (depth 2)

-- TEST 3 - PERFORMANCE

m :: Int
n :: Int

m = 8
n = 8

newtype N = N Int deriving (Show)

instance Num N where
  (+) (N x) (N y) = N (x + y)
  (*) (N x) (N y) = N (x * y)
  (-) (N x) (N y) = N (x - y)
  abs (N x)       = N (abs x)
  signum (N x)    = N (signum x)
  fromInteger x   = N (fromInteger x)

intAdjs :: N -> [N]
intAdjs i = take m $ iterate (1+) (10 * i)

intShow1 :: N -> T.Text
intShow1 (N i) = T.pack $ show i

test31 :: IO () -- DIRECT IO
test31 = PT.printTree (N 1) intAdjs intShow1 (depth n)

test32 :: IO () -- BUILDING TEXT AND THEN IO
test32 =
  P.printLn $ P.toLazilyBuiltText $
    PT.printTree (N 1) intAdjs intShow1 (depth n)
