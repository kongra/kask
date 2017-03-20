{-# LANGUAGE      Safe #-}

module Test009 where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
-- import qualified Data.Map.Strict as Map
-- import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict as Map

data Msg = Msg !Int !ByteString.ByteString

type Chan = Map.Map Int ByteString.ByteString

message :: Int -> Msg
message n = Msg n (ByteString.replicate 1024 (fromIntegral n))

pushMsg :: Chan -> Msg -> IO Chan
pushMsg chan (Msg msgId msgContent) =
  Exception.evaluate $
    let
      inserted = Map.insert msgId msgContent chan
    in
      if 200000 < Map.size inserted
      then Map.delete (msgId - 200000) inserted
      else inserted

test1 :: IO ()
test1 = Monad.foldM_ pushMsg Map.empty (map message [1..1000000])
