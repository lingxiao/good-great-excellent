module Paths_good_great_excellent (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/lingxiao/Documents/research/code/good-great-excellent/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/bin"
libdir     = "/Users/lingxiao/Documents/research/code/good-great-excellent/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/lib/x86_64-osx-ghc-7.10.3/good-great-excellent-0.1.0.0-DmvfzrsRlTTEG4TgbQ9uXe"
datadir    = "/Users/lingxiao/Documents/research/code/good-great-excellent/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/share/x86_64-osx-ghc-7.10.3/good-great-excellent-0.1.0.0"
libexecdir = "/Users/lingxiao/Documents/research/code/good-great-excellent/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/libexec"
sysconfdir = "/Users/lingxiao/Documents/research/code/good-great-excellent/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "good_great_excellent_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "good_great_excellent_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "good_great_excellent_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "good_great_excellent_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "good_great_excellent_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
