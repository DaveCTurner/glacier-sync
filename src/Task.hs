{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Task where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import GHC.Conc (labelThread)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time
import Control.Monad

data Task = Task
  { taskId              :: Integer
  , taskDescription     :: Value
  , taskStartTime       :: UTCTime
  , taskCancel          :: STM ()
  , taskAwaitCompletion :: STM ()
  , taskGetStatus       :: STM Value
  , taskGetCancelled    :: STM Bool
  }

instance ToJSON Task where
  toJSON Task{..} = object [ "id" .= taskId, "description" .= taskDescription ]

data TaskStatus = TaskStatus
  { taskStatusId          :: Integer
  , taskStatusDescription :: Value
  , taskStatusStartTime   :: UTCTime
  , taskStatusStatus      :: Value
  , taskStatusCancelled   :: Bool
  }

instance ToJSON TaskStatus where
  toJSON TaskStatus{..} = object [ "id"          .= taskStatusId
                                 , "description" .= taskStatusDescription
                                 , "status"      .= taskStatusStatus
                                 , "start_time"  .= taskStatusStartTime
                                 , "cancelled"   .= taskStatusCancelled
                                 ]

data TaskCancelled = TaskCancelled deriving (Show, Eq)

data TaskInner = TaskInner
  { taskSetStatus      :: forall a. ToJSON a => a -> STM (Maybe TaskCancelled)
  }

data TaskManager = TaskManager
  { taskManagerNextId :: TVar Integer
  , taskManagerTasks  :: TVar (HM.HashMap Integer Task)
  }

newTaskManager :: IO TaskManager
newTaskManager = TaskManager <$> newTVarIO 0 <*> newTVarIO HM.empty

newTaskId :: TaskManager -> STM Integer
newTaskId TaskManager{..} = readTVar taskManagerNextId <* modifyTVar' taskManagerNextId (+1)

addTask :: TaskManager -> Task -> STM ()
addTask TaskManager{..} task = modifyTVar' taskManagerTasks $ HM.insert (taskId task) task

removeTask :: TaskManager -> Task -> STM ()
removeTask TaskManager{..} task = modifyTVar' taskManagerTasks $ HM.delete (taskId task)

getTasks :: TaskManager -> STM [Task]
getTasks TaskManager{..} = HM.elems <$> readTVar taskManagerTasks

getTaskStatus :: TaskManager -> Integer -> STM (Maybe TaskStatus)
getTaskStatus TaskManager{..} taskId' = do
  maybeTask <- HM.lookup taskId' <$> readTVar taskManagerTasks
  case maybeTask of
    Nothing -> return Nothing
    Just Task{..} -> Just <$> (TaskStatus taskId taskDescription taskStartTime <$> taskGetStatus <*> taskGetCancelled)

cancelTask :: TaskManager -> Integer -> IO Bool
cancelTask TaskManager{..} taskId'
    = join $ atomically $ maybe notFound cancelAndAwait =<< HM.lookup taskId' <$> readTVar taskManagerTasks
  where
  notFound = return $ return False
  cancelAndAwait Task{..} = do
    taskCancel
    return $ do
      atomically taskAwaitCompletion
      return True

forkTask :: (ToJSON description, ToJSON status)
  => TaskManager -> description -> status -> (TaskInner -> IO ()) -> IO Task
forkTask taskManager description initialStatus go = mask $ \restore -> do
  taskStartTime <- getCurrentTime
  (task, taskAction, finishTask) <- atomically $ do
    taskStatusVar    <- newTVar $ toJSON initialStatus
    taskCancelledVar <- newTVar False
    taskFinishedVar  <- newTVar False
    taskId           <- newTaskId taskManager

    let finishTask message = atomically $ do
          writeTVar taskFinishedVar True
          writeTVar taskStatusVar $ object [ "status"  .= String "finished"
                                           , "message" .= String message
                                           ]
          removeTask taskManager task

        taskDescription     = toJSON description
        taskCancel          = writeTVar taskCancelledVar True
        taskAwaitCompletion = check =<< readTVar taskFinishedVar
        taskGetStatus       = readTVar taskStatusVar
        taskGetCancelled    = readTVar taskCancelledVar

        task = Task{..}

        taskAction = restore $ go TaskInner
          { taskSetStatus      = \v -> do
              taskCancelled <- readTVar taskCancelledVar
              if taskCancelled
                then return $ Just TaskCancelled
                else do
                  writeTVar taskStatusVar $ toJSON v
                  return Nothing
          }

    addTask taskManager task

    return (task, taskAction, finishTask)

  tid <- forkIO (restore taskAction
                 `finally` finishTask "finished")
         `onException`     finishTask "failed to start"

  labelThread tid $ TL.unpack $ TL.decodeUtf8 $ encode $ toJSON description

  return task
