module Paths_number_theory (
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
version = Version {versionBranch = [0,2,0,5], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/brian/.cabal/bin"
libdir     = "/home/brian/.cabal/lib/x86_64-linux-ghc-7.8.3/number-theory-0.2.0.5"
datadir    = "/home/brian/.cabal/share/x86_64-linux-ghc-7.8.3/number-theory-0.2.0.5"
libexecdir = "/home/brian/.cabal/libexec"
sysconfdir = "/home/brian/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "number_theory_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "number_theory_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "number_theory_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "number_theory_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "number_theory_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
