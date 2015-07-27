module Paths_astar (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,2,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sulami/build/maze1/.cabal-sandbox/bin"
libdir     = "/home/sulami/build/maze1/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.1/astar_HYT6e6Q1WDEBnNS4x0ROH0"
datadir    = "/home/sulami/build/maze1/.cabal-sandbox/share/x86_64-linux-ghc-7.10.1/astar-0.2.0.0"
libexecdir = "/home/sulami/build/maze1/.cabal-sandbox/libexec"
sysconfdir = "/home/sulami/build/maze1/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "astar_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "astar_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "astar_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "astar_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "astar_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
