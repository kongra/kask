module Main (main) where

import           Kask.Time
import qualified System.Clock as Clock

main :: IO ()
main = do
  let coll = [1 .. 10000000] :: [Integer]
  let s    = sum coll
  (value, msecs) <- withMsecs Clock.Monotonic s
  print msecs
  print value
