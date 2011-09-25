{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Control.Applicative
import Control.Monad
import Data.List (groupBy, partition, sortBy)
import Data.Monoid
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Text.Read
import Data.Time
import Database.Persist.GenericSql.Raw (SqlPersist)
import Yesod.Auth (maybeAuthId)
import Yesod.Handler
import Foundation


redirectTemporary :: HasReps a => YesodoroRoute -> Handler a
redirectTemporary = redirect RedirectTemporary


-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = maybeAuthId >>= getRootR' where
  getRootR' :: Maybe UserId -> Handler RepHtml
  getRootR' Nothing = defaultLayout $ do
        setTitle "yesodoro"
        addWidget $(widgetFile "homepage")
  getRootR' (Just _) = redirectTemporary TasksR


authed :: HasReps a => (UserId -> Handler a) -> Handler a
authed handler = maybeAuthId >>= maybe (redirectTemporary RootR) handler


authedTask :: HasReps a => ((TaskId, Task) -> Handler a) -> TaskId -> Handler a
authedTask handler taskId = authed (\userId -> do
    maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
    case maybeAuthedTask of
      Just task -> handler task
      Nothing -> redirectTemporary TasksR)



getTasksR :: Handler RepHtml
getTasksR = authed (\userId -> do
  tasks <- runDB $ userTasks userId

  estimates <- runDB $ mapM (taskEstimates . fst) tasks
  let tasksEstimates :: [(TaskId, (Task, [(EstimateId, Estimate)]))]
      tasksEstimates = (map fst tasks) `zip` ((map snd tasks) `zip` estimates)

  timeZone <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  let taskTodoToday :: Task -> Bool
      taskTodoToday = taskTodo timeZone now

  let (unsortedDone, pending) = partition (taskDone . fst . snd) tasksEstimates
  let done = reverse $ sortBy (compareBy $ taskDoneAt . fst . snd) unsortedDone
  let (unsortedTodo, unsortedLater) = partition (taskTodoToday . fst . snd) pending
  let todo = sortBy (compareBy $ taskOrder . fst . snd) unsortedTodo
  let later = sortBy (compareBy $ taskOrder . fst . snd) unsortedLater

  let doneByDay = groupByEq (fromJust . taskDoneDay timeZone . fst . snd) done
  ((_, taskWidget), enctype) <- generateFormPost taskForm
  defaultLayout $ do
      setTitle "tasks"
      addWidget $(widgetFile "tasks")) where

  userTasks userId = selectList [TaskUser ==. userId] [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...
  taskEstimates taskId = selectList [EstimateTask ==. taskId] []
  taskTr taskTodoToday (taskId, (task, estimates)) = $(widgetFile "tasks/task-tr")
  estimatedRemaining :: (Task, [(EstimateId, Estimate)]) -> Int
  estimatedRemaining (_, []) = 0
  estimatedRemaining (task, ((_, estimate) : _)) = (estimatePomos estimate - taskPomos task) `max` 0


oneButton :: Text -> YesodoroRoute -> Widget
oneButton label route = [whamlet|
  <form method=POST action=@{route}
    <button>#{label}
|]

taskForm :: Html -> Form Yesodoro Yesodoro (FormResult NewTask, Widget)
taskForm = renderDivs $ NewTask <$> areq textField "Title" Nothing

postTasksR :: Handler RepHtml
postTasksR = authed (\userId -> do
  ((result, taskWidget), _) <- runFormPost taskForm
  case result of
    FormSuccess task -> do
      runDB $ createTaskAtBottom userId task
      redirectTemporary TasksR
    _ -> undefined) -- TODO


postCompleteTaskR :: TaskId -> Handler RepHtml
postCompleteTaskR = authedTask (\(taskId, _) -> do
  now <- liftIO getCurrentTime
  updateAndRedirectR TasksR [TaskDoneAt =. Just now] taskId)

postRestartTaskR :: TaskId -> Handler RepHtml
postRestartTaskR = updateAndRedirectR TasksR [TaskDoneAt =. Nothing]

updateAndRedirectR :: HasReps a => YesodoroRoute -> [Update Task] -> TaskId -> Handler a
updateAndRedirectR route updates = authedTask (\(taskId, _) -> do
  runDB $ update taskId updates
  redirectTemporary route)

setTaskDonenessRoute :: (TaskId, Task) -> YesodoroRoute
setTaskDonenessRoute (taskId, task) = route taskId
  where route | taskDone task = RestartTaskR
              | otherwise     = CompleteTaskR

setTaskDonenessButton :: TaskId -> Task -> Widget
setTaskDonenessButton taskId task = oneButton action (route taskId)
  where action = taskActionName task
        route | taskDone task = RestartTaskR
              | otherwise     = CompleteTaskR

postDeleteTaskR :: TaskId -> Handler RepHtml
postDeleteTaskR = authedTask (\(taskId, _) -> do
  runDB $ deleteWhere [EstimateTask ==. taskId]
  runDB $ delete taskId
  redirectTemporary TasksR)

postTaskAddPomoR :: TaskId -> Handler RepHtml
postTaskAddPomoR = updateAndRedirectR TasksR [TaskPomos +=. 1]

postTaskAddEstimateR :: TaskId -> Handler RepHtml
postTaskAddEstimateR = authedTask (\(taskId, _) -> do
  pomosParam <- lookupPostParam "pomos"
  let pomos = do
      param <- maybeToEither "no pomos" $ pomosParam
      (num, _) <- decimal param
      return num
  case pomos of
    (Right numPomos) -> do
      runDB $ insert $ Estimate taskId numPomos
      redirectTemporary TasksR
    Left msg -> error msg) -- TODO

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither msg Nothing = Left msg
maybeToEither _ (Just v) = Right v


eqUnder :: Eq b => (a -> b) -> a -> a -> Bool
eqUnder f a b = f a == f b

groupByEq :: Eq g => (a -> g) -> [a] -> [(g, [a])]
groupByEq f as = zip gs groups where
  groups = groupBy (eqUnder f) as
  gs = map (f . head) groups


postRaiseTaskR :: TaskId -> Handler RepHtml
postRaiseTaskR = reorderTaskR Up

postLowerTaskR :: TaskId -> Handler RepHtml
postLowerTaskR = reorderTaskR Down

reorderTaskR :: Direction -> TaskId -> Handler RepHtml
reorderTaskR direction = authedTask (\task -> do
  utcNow <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  let endOfToday = locally tz localEndOfDay utcNow

  runDB $ reorderTask direction endOfToday task
  redirectTemporary TasksR)


postPostponeTaskR :: TaskId -> Handler RepHtml
postPostponeTaskR = authedTask (\task -> do
  runDB $ postponeTask task
  redirectTemporary TasksR)


postActivateTaskR :: TaskId -> Handler RepHtml
postActivateTaskR = authedTask (\(taskId, _) -> do
  now <- liftIO getCurrentTime
  runDB $ activateTask now taskId
  redirectTemporary TasksR)
