module Main (main) where

collatzTrans :: Integral a => a -> a
collatzTrans n = if even n then n `div` 2 else 3 * n + 1

collatzSeq :: Integral a => a -> [a]
collatzSeq = takeWhile (/= 1) . iterate collatzTrans

collatzLen :: Integral a => a -> Int
collatzLen = length . collatzSeq

euler14 :: Integral a => a -> (a, Int)
euler14 n = loop 0 0 1 where
  loop maxLen j i =
    if i == n then
      (j, maxLen)
    else
      let len = collatzLen i in
        if len > maxLen then
          loop len i (i+1)
        else
          loop maxLen j (i+1)

type Euler14Result = (Int, Int)

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
