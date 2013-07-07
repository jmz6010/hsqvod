{-# LANGUAGE RecordWildCards, DeriveGeneric, DefaultSignatures #-}

module Task
    ( taskFromUri
    , newManager
    , runTask
    , pauseTask
    , toggleTask
    , deleteTask
    , findTask
    , insertTask
    , Task(..)
    , Manager
    ) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Exception (tryJust)
import Control.Monad

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Ratio
import Data.Serialize
import GHC.Generics

import System.Posix.Files (fileSize, getFileStatus)
import System.Process (ProcessHandle, runCommand, terminateProcess)
import System.FilePath (FilePath, (</>), dropExtension, addExtension)
import System.Directory (createDirectory, copyFile, renameFile, doesFileExist, removeDirectoryRecursive)
import System.IO.Error (isAlreadyExistsError)

import Text.Regex.TDFA

import Config

type URI = String
type MovieCheckSum = String

data MovieInfo = MovieInfo {
      movieSize     :: Integer
    , movieChecksum :: MovieCheckSum
    , movieFileName :: String
} deriving (Show, Generic)

data TaskState = Running | Paused | Stopped | Stalled | Completed
                 deriving (Eq, Show, Generic)

data Task = Task {
      tMovieInfo   :: MovieInfo
    , tCompleted   :: Integer
    , tState       :: TaskState
    , tProcess     :: Maybe ProcessHandle
    , tThreadId    :: Maybe ThreadId
    , tCurrentRate :: Ratio Integer
    , tAverageRate :: Ratio Integer
    , tTotalTime   :: Integer
}

data SerializableTask = SerializableTask {
      stMovieInfo :: MovieInfo
    , stCompleted :: Integer
    , stState     :: TaskState
    } deriving (Generic)


-- sansRuntimeInfo :: Task -> SerializableTask
-- sansRuntimeInfo Task{..} = SerializableTask tMovieInfo tCompleted tState

-- recoverTask :: SerializableTask -> Task
-- recoverTask SerializableTask{..} = Task stMovieInfo stCompleted stState Nothing Nothing

instance Serialize MovieInfo
instance Serialize TaskState
instance Serialize SerializableTask

instance Show Task where
    show Task{..} = let movieName = movieFileName tMovieInfo
                        progress = concat [ show tCompleted, "/", show (movieSize tMovieInfo) ]
                    in concat [ movieName, "\t", show tState, "\t", progress ]

type Manager = IntMap (TVar Task)

newManager :: Manager
newManager = IntMap.empty

findTask :: Manager -> Int -> Maybe (TVar Task)
findTask = flip IntMap.lookup

parseURI :: URI -> Maybe MovieInfo
parseURI uri = mk (uri =~ pat :: (String, String, String, [String]))
    where pat = "^qvod://([[:digit:]]+)\\|([[:alnum:]]+)\\|(.+\\.[[:alnum:]]+)\\|$"
          mk (_, _, _, []) = Nothing
          mk (_, _, _, length:checksum:filename:[]) = Just $ MovieInfo (read length :: Integer) checksum filename

newTask :: MovieInfo -> Task
newTask mi = Task {
               tMovieInfo = mi
             , tCompleted = 0
             , tState = Stopped
             , tProcess = Nothing
             , tThreadId = Nothing
             , tCurrentRate = 0 % 100
             , tAverageRate = 0 % 100
             , tTotalTime = 0
}

taskFromUri :: URI -> Maybe Task
taskFromUri = liftM newTask . parseURI

taskDirectory :: Task -> FilePath
taskDirectory Task{..} = let dirName = dropExtension (movieFileName tMovieInfo)
                         in appDir </> dirName

taskDownloader :: Task -> FilePath
taskDownloader task@Task{..} = taskDirectory task </> movieDownloader tMovieInfo

movieDownloader :: MovieInfo -> FilePath
movieDownloader MovieInfo{..} = concat [movieChecksum, "+", movieFileName, "_", movieChecksum, ".exe"]

taskFile :: Task -> FilePath
taskFile task@Task{..} = taskDirectory task </> fileName
    where fileName = movieChecksum tMovieInfo ++ "+" ++ movieFileName tMovieInfo

destFile :: Task -> FilePath
destFile Task{..} = destDir </> movieFileName tMovieInfo

taskPartialFile :: Task -> FilePath
taskPartialFile = flip addExtension "!qd" . taskFile

insertTask :: Manager -> TVar Task -> Manager
insertTask mng tvt = let currId = 1 + IntMap.size mng
                     in IntMap.insert currId tvt mng

prepareTask :: Task -> IO ()
prepareTask task = do
  let taskDir = taskDirectory task
      taskExec = taskDownloader task

  -- make a folder to store temporary task files
  tryJust (guard . isAlreadyExistsError) (createDirectory taskDir)

  -- copy the downloader over if we haven't done it already
  exists <- doesFileExist taskExec
  unless exists $ copyFile execPath taskExec

runTaskProcess :: TVar Task -> IO ()
runTaskProcess tvt = do
  task <- readTVarIO tvt
  prepareTask task
  handle <- runCommand $ "wine '" ++ taskDownloader task ++ "'"
  atomically $ modifyTVar' tvt (\t -> t { tProcess = Just handle, tState = Stalled })

pauseTask :: TVar Task -> IO ()
pauseTask tvt = do
  task <- readTVarIO tvt
  case tThreadId task of
    Nothing -> return ()
    Just tid -> killThread tid
  maybe (putStrLn "Task not started") (\handle -> terminateProcess handle >> setPause) (tProcess task)
    where setPause = atomically $ modifyTVar' tvt (\t -> t { tState = Paused })

toggleTask :: TVar Task -> IO ()
toggleTask tvt = readTVarIO tvt >>= \t -> if tState t `elem` [ Running, Stalled ]
                                          then pauseTask tvt
                                          else runTask tvt

resumeTask :: Task -> IO ()
resumeTask = undefined

-- delete a task and its temporary files
deleteTask :: TVar Task -> IO ()
deleteTask tvt = pauseTask tvt >> readTVarIO tvt >>= removeDirectoryRecursive . taskDirectory

runTask :: TVar Task -> IO ()
runTask tvt = do
  threadId <- forkIO $ do
                     task <- readTVarIO tvt
                     putStrLn $ "Downloading" ++ show task ++ "..."
                     runTaskProcess tvt
                     loop tvt
  atomically $ modifyTVar' tvt (\t -> t { tThreadId = Just threadId })

      where loop tv = do threadDelay (threadInterval * 1000000)
                         completed <- (taskCompleted <=< readTVarIO) tv
                         if completed then finishTask tv else loop' tv >> loop tv
            loop' tv = do task <- readTVarIO tv
                          let lastSize = tCompleted task
                              partialFile = taskPartialFile task
                              totalTime = tTotalTime task + toInteger threadInterval
                          exists <- doesFileExist partialFile
                          when exists $ do
                            newSize <- liftM (toInteger . fileSize) . getFileStatus $ partialFile
                            let currState = if newSize - lastSize > 0 then Running else Stalled
                            atomically $ modifyTVar' tv (\t -> t { tTotalTime   = totalTime
                                                                 , tState       = currState
                                                                 , tCompleted   = newSize
                                                                 , tCurrentRate = (newSize - lastSize) % toInteger threadInterval
                                                                 , tAverageRate = newSize % totalTime})

taskCompleted :: Task -> IO Bool
taskCompleted = doesFileExist . taskFile

killTask :: TVar Task -> TaskState -> IO ()
killTask tvt st = do
  handle <- liftM tProcess . readTVarIO $ tvt
  maybe (error "Attempting to kill a nonexistent process") terminateProcess handle
  atomically $ modifyTVar' tvt (\t -> t { tState = st })

finishTask :: TVar Task -> IO ()
finishTask tvt = do
  killTask tvt Completed
  task <- readTVarIO tvt
  renameFile (taskFile task) (destFile task)
