{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Task where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Exception

data Task = Task
  { taskId           :: Integer
  , taskDescription  :: Value
  -- TODO add start time
  , taskCancel       :: STM ()
  , taskGetStatus    :: STM Value
  }

instance ToJSON Task where
  toJSON Task{..} = object [ "id" .= taskId, "description" .= taskDescription ]

data TaskStatus = TaskStatus
  { taskStatusId          :: Integer
  , taskStatusDescription :: Value
  , taskStatusStatus      :: Value
  }

instance ToJSON TaskStatus where
  toJSON TaskStatus{..} = object [ "id"          .= taskStatusId
                                 , "description" .= taskStatusDescription
                                 , "status"      .= taskStatusStatus
                                 ]

data TaskCancelled = TaskCancelled deriving (Show, Eq)

data TaskInner = TaskInner
  { taskSetStatus      :: forall a. ToJSON a => a -> STM (Maybe TaskCancelled)
  , taskAwaitCancelled :: STM ()
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
    Just Task{..} -> Just . TaskStatus taskId taskDescription <$> taskGetStatus

cancelTask :: TaskManager -> Integer -> IO Bool
cancelTask TaskManager{..} taskId' = do
  maybeTask <- atomically $ HM.lookup taskId' <$> readTVar taskManagerTasks
  case maybeTask of
    Nothing -> return False
    Just Task{..} -> do
      atomically taskCancel
      -- TODO await completion
      -- TODO shouldn't remove task from list until completed
      return True

forkTask :: (ToJSON description, ToJSON status)
  => TaskManager -> description -> status -> (TaskInner -> IO ()) -> IO Task
forkTask taskManager description initialStatus go = mask $ \restore -> do
  (task, taskStatusVar, taskCancelledVar, cleanupTask) <- atomically $ do
    taskStatusVar    <- newTVar $ toJSON initialStatus
    taskCancelledVar <- newTVar False
    taskId           <- newTaskId taskManager

    let cleanupTask message = do
          writeTVar taskCancelledVar True
          writeTVar taskStatusVar $ object [ "status" .= String "finished"
                                           , "message" .= String message
                                           ]
          removeTask taskManager task

        taskDescription = toJSON description
        taskCancel      = cleanupTask "cancelled"
        taskGetStatus   = readTVar taskStatusVar

        task = Task{..}

    addTask taskManager task

    return (task, taskStatusVar, taskCancelledVar, atomically . cleanupTask)
    -- TODO label forked thread
  void $ forkIO (restore (go TaskInner
      { taskSetStatus      = \v -> do
          taskCancelled <- readTVar taskCancelledVar
          if taskCancelled
            then return $ Just TaskCancelled
            else do
              writeTVar taskStatusVar $ toJSON v
              return Nothing

      , taskAwaitCancelled = check =<< readTVar taskCancelledVar
      }) `finally` cleanupTask "finished") `onException` cleanupTask "failed to start"

  return task