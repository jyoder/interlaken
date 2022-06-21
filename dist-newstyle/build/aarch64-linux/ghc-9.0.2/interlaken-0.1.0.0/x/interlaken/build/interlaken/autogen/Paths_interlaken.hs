{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_interlaken (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/john/.cabal/bin"
libdir     = "/home/john/.cabal/lib/aarch64-linux-ghc-9.0.2/interlaken-0.1.0.0-inplace-interlaken"
dynlibdir  = "/home/john/.cabal/lib/aarch64-linux-ghc-9.0.2"
datadir    = "/home/john/.cabal/share/aarch64-linux-ghc-9.0.2/interlaken-0.1.0.0"
libexecdir = "/home/john/.cabal/libexec/aarch64-linux-ghc-9.0.2/interlaken-0.1.0.0"
sysconfdir = "/home/john/.cabal/etc"

getBinDir     = catchIO (getEnv "interlaken_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "interlaken_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "interlaken_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "interlaken_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "interlaken_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "interlaken_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
