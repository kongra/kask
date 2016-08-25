module Main (main) where

import qualified Kask.Logic.Kleene as Kleene

main :: IO ()
main = print (Kleene.not Kleene.True)
