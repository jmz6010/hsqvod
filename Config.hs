
module Config
    ( homeDir
    , configFile
    , appDir
    , destDir
    , execPath
    , threadInterval
    ) where

import System.FilePath (FilePath, (</>))
import System.Directory (getHomeDirectory)
import System.IO.Unsafe (unsafePerformIO)

homeDir :: FilePath
homeDir = unsafePerformIO getHomeDirectory

destDir :: FilePath
destDir = homeDir </> "visibles"

configFile :: FilePath
configFile = homeDir </> ".hsqvod"

appDir :: FilePath
appDir = homeDir </> ".hsqvod.d"

execPath :: FilePath
execPath = appDir </> "qvoddownloader.exe"

threadInterval :: Int
threadInterval = 3
