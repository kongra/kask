{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_kask_base (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kongra/Devel/Projects/Present/Haskell/01-kask-base/.cabal-sandbox/bin"
libdir     = "/home/kongra/Devel/Projects/Present/Haskell/01-kask-base/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.1.20160725/kask-base-0.1.0.0-1nIMjO9zSxDCrSbl8Jltx"
datadir    = "/home/kongra/Devel/Projects/Present/Haskell/01-kask-base/.cabal-sandbox/share/x86_64-linux-ghc-8.0.1.20160725/kask-base-0.1.0.0"
libexecdir = "/home/kongra/Devel/Projects/Present/Haskell/01-kask-base/.cabal-sandbox/libexec"
sysconfdir = "/home/kongra/Devel/Projects/Present/Haskell/01-kask-base/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "kask_base_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "kask_base_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "kask_base_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "kask_base_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "kask_base_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
