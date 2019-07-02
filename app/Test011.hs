module Test011 where

import RIO
import Prelude (print)
import Data.Maybe (fromJust)

type    Value   = Int
newtype Wrapper = Wrapper Value deriving Show;

runWrapper :: Wrapper -> Value
runWrapper (Wrapper i) = i

gen :: Wrapper -> Maybe Wrapper
gen (Wrapper n) = Just (Wrapper (n * 3 + 1))

f :: Value -> Value
f = runWrapper . fromJust . gen . Wrapper

experiment :: Wrapper -> IO ()
experiment (Wrapper n) = do
  print $ "Performing experiment " ++ show n
  print $ loop (Wrapper 0) (Wrapper 1) where
    loop (Wrapper s) (Wrapper i) =
      if i == (1000000000 + n) then
        Wrapper s
      else
        loop (Wrapper (s + f i)) (Wrapper (i + 1))

test1 :: IO ()
test1 = do
  experiment (Wrapper 10)
  experiment (Wrapper 11)
  experiment (Wrapper 12)
  experiment (Wrapper 13)
  experiment (Wrapper 14)
  experiment (Wrapper 15)
  experiment (Wrapper 16)
  experiment (Wrapper 17)
  experiment (Wrapper 18)
  experiment (Wrapper 19)
