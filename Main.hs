module Main (main) where

import Control.Concurrent.STM (TVar, readTVarIO, newTVarIO)
import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List.Split (splitOn)
import System.Directory (findExecutable, createDirectoryIfMissing, doesFileExist)
import System.Exit (exitSuccess)

import Config
import Task

data Callback = CallbackA (String -> Manager -> IO Manager)
              | CallbackB (String -> Manager -> IO ())

actionMap :: HashMap String Callback
actionMap = HashMap.fromList [("l", listAllTasks), ("a", addTask),
                              ("d", deleteTaskById), ("p", toggleTaskById),
                              ("q", quit), ("h", help)]

main :: IO ()
main = do
  checkForWine
  createDirectoryIfMissing False appDir
  exists <- doesFileExist execPath
  unless exists (error $ "Please put qvoddownloader.exe into " ++ appDir)
  mainLoop newManager

-- XXX: catch IO errors
mainLoop mng = do
  cmd:rest <- splitOn " " `fmap` getLine
  let rest' = case rest of
                [] -> ""
                xs -> concat xs
  maybe (putStrLn ("Invalid command: " ++ cmd) >> mainLoop mng)
        (\c -> case c of
                 CallbackB cb -> cb rest' mng >> mainLoop mng
                 CallbackA cb' -> cb' rest' mng >>= mainLoop)
        (HashMap.lookup cmd actionMap)

checkForWine :: IO ()
checkForWine = findExecutable "wine" >>= maybe (error "Wine not found") (putStrLn . ("Using Wine from " ++))

listAllTasks :: Callback
listAllTasks = CallbackB $ \_ mng -> case IntMap.toAscList mng of
                     [] -> putStrLn "No active tasks."
                     tasks -> do
                       putStrLn $ show (length tasks) ++ " tasks:"
                       mapM showNumberedTask tasks >>= mapM_ putStrLn

showNumberedTask :: (IntMap.Key, TVar Task) -> IO String
showNumberedTask (no, tvt) = do
  tvs <- (liftM show . readTVarIO) tvt
  return $ show no ++ " :  " ++ tvs

addTask :: Callback
addTask = CallbackA $ \uri mng ->
  maybe (putStrLn "Failed to add task, invalid URI" >> return mng)
        (newTVarIO >=> \t -> runTask t >> return (insertTask mng t))
        (taskFromUri uri)

-- toggle paused state
toggleTaskById :: Callback
toggleTaskById = CallbackB $ \ids mng ->
  let id = read ids :: IntMap.Key
      tvt = findTask mng id
  in maybe (putStrLn $ "No such task: task numbered " ++ ids ++ " does not exist.")
           toggleTask
           tvt

deleteTaskById :: Callback
deleteTaskById = CallbackA $ \ids mng ->
  let id = read ids :: IntMap.Key
      tvt = findTask mng id
  in maybe (putStrLn ("No such task: task numbered " ++ ids ++ " does not exist.") >> return mng)
           (\t -> deleteTask t >> return (IntMap.delete id mng))
           tvt

quit :: Callback
quit = CallbackB $ \_ mng -> putStrLn "Bye" >> exitSuccess

help :: Callback
help = CallbackB $ \_ _ -> do
  putStrLn "HsQvod is a simple Qvod link downloader written in Haskell, "
  putStrLn "which is essentially a wrapper around Wine and the native Windows downloader."
  putStrLn "The code is Public Domain and can be found at https://github.com/qingl/hsqvod/"
