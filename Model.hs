{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs, OverloadedStrings, FlexibleContexts #-}
module Model where

import Yesod
import Data.Maybe (isJust)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time
import Database.Persist.GenericSql.Raw (SqlPersist)
import Text.Blaze (ToHtml)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")


compareBy :: Ord a => (b -> a) -> b -> b -> Ordering
compareBy f x y = compare (f x) (f y)


newtype TaskState = TaskState Text
  deriving (ToHtml, IsString)


data NewTask = NewTask { newTaskTitle :: Text } deriving (Show)

newTask :: UserId -> UTCTime -> Int -> NewTask -> Task
newTask uid scheduledFor order (NewTask title) = Task uid title 0 scheduledFor Nothing order

createTaskAtBottom :: PersistBackend SqlPersist m => UserId -> NewTask -> SqlPersist m TaskId
createTaskAtBottom userId task = do
  now <- liftIO getCurrentTime
  maybeLastTask <- selectFirst [TaskUser ==. userId] [Desc TaskOrder]
  let lastOrder = maybe 0 (taskOrder . snd) maybeLastTask
  insert $ newTask userId now (succ lastOrder) task


data Direction = Up | Down deriving (Show, Enum, Bounded)

nextTask :: PersistBackend SqlPersist m => Direction -> UTCTime -> Task -> SqlPersist m (Maybe (TaskId, Task))
nextTask direction endOfToday task = selectFirst
    [ TaskUser ==. (taskUser task)
    , (orderConstraint direction) TaskOrder (taskOrder task)
    , TaskDoneAt ==. Nothing
    , scheduledForConstraint TaskScheduledFor
    ] [(order direction) TaskOrder]
  where
    orderConstraint Up = (<.)
    orderConstraint Down = (>.)
    order Up = Desc
    order Down = Asc
    scheduledForConstraint | taskScheduledFor task <= endOfToday = (<=. endOfToday)
                           | otherwise                           = (>. endOfToday)

reorderTask :: PersistBackend SqlPersist m => Direction -> UTCTime -> (TaskId, Task) -> SqlPersist m ()
reorderTask direction endOfToday (taskId, task) = do
  maybeNext <- nextTask direction endOfToday task
  case maybeNext of
    Nothing -> return ()
    Just (nextId, next) -> do
      update taskId [TaskOrder =. (-1)] -- temporary value
      update nextId [TaskOrder =. (taskOrder task)]
      update taskId [TaskOrder =. (taskOrder next)]


taskDone :: Task -> Bool
taskDone = isJust . taskDoneAt


taskTodo :: TimeZone -> UTCTime -> Task -> Bool
taskTodo tz now = (<= today) . taskScheduledForDay tz
  where today = utcToLocalDay tz now

taskOverdue :: TimeZone -> UTCTime -> Task -> Bool
taskOverdue tz now task = not (taskDone task) && taskScheduledForDay tz task < today
  where today = utcToLocalDay tz now

utcToLocalDay :: TimeZone -> UTCTime -> Day
utcToLocalDay tz = localDay . utcToLocalTime tz

localEndOfDay :: LocalTime -> LocalTime
localEndOfDay time = time { localTimeOfDay = TimeOfDay 23 59 59 }

locally :: TimeZone -> (LocalTime -> LocalTime) -> UTCTime -> UTCTime
locally tz f = localTimeToUTC tz . f . utcToLocalTime tz

tomorrow :: UTCTime -> UTCTime
tomorrow = addUTCTime oneDay
  where oneDay = 24 * 60 * 60

taskDoneDay :: TimeZone -> Task -> Maybe Day
taskDoneDay tz = fmap (utcToLocalDay tz) . taskDoneAt

taskScheduledForDay :: TimeZone -> Task -> Day
taskScheduledForDay tz = utcToLocalDay tz . taskScheduledFor

instance ToHtml Day where
  toHtml = toHtml . show

taskState :: Task -> TaskState
taskState task = if taskDone task then "done" else "pending"

taskActionName :: Task -> Text
taskActionName task | taskDone task = "Restart"
                    | otherwise     = "Complete"

estimateOptions :: [Int]
estimateOptions = [2 ^ x | x <- [0 .. 4]]


postponeTask :: PersistBackend SqlPersist m => (TaskId, Task) -> SqlPersist m ()
postponeTask (taskId, task) = update taskId [TaskScheduledFor =. tomorrow (taskScheduledFor task)]


activateTask :: PersistBackend SqlPersist m => UTCTime -> TaskId -> SqlPersist m ()
activateTask now taskId = update taskId [TaskScheduledFor =. now]
