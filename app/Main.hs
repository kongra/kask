module Main (main) where

import           RIO
-- import Criterion.Main

-- import qualified Test001
-- import qualified Test002
-- import qualified Test003
-- import qualified Test004
-- import qualified Test005
-- import qualified Test006
-- import qualified Test007
-- import qualified Test008
-- import qualified Test009
import qualified Test010
-- import qualified Test011
-- import qualified Test012

main :: IO ()
-- main = defaultMain [
--   bgroup "fib" [ bench "100"    $ whnf Test012.tescik100 100
--                , bench "100000" $ whnf Test012.tescik100 1000
--                --, bench "79"  $ whnf Test012.tescik20 79
--                -- , bench "430" $ whnf Test012.tescik200 430
--                ]]

main =
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
  Test010.test1
