{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Control.Applicative
import Control.Monad
import Data.List (groupBy, partition)
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
getRootR = maybeAuth >>= getRootR' where
  getRootR' :: Maybe (UserId, User) -> Handler RepHtml
  getRootR' Nothing = defaultLayout $ do
        setTitle "yesodoro"
        addWidget $(widgetFile "homepage")
  getRootR' (Just _) = redirectTemporary TasksR


getTasksR :: Handler RepHtml
getTasksR = maybeAuth >>= getTasksR' where
  getTasksR' :: Maybe (UserId, User) -> Handler RepHtml

  getTasksR' Nothing = redirectTemporary RootR

  getTasksR' (Just (userId, user)) = do
    tasks <- runDB $ userTasks userId

    estimates <- runDB $ mapM (taskEstimates . fst) tasks
    let tasksEstimates :: [(TaskId, (Task, [(EstimateId, Estimate)]))]
        tasksEstimates = (map fst tasks) `zip` ((map snd tasks) `zip` estimates)

    let (done, pending) = partition (taskDone . fst . snd) tasksEstimates

    timeZone <- liftIO getCurrentTimeZone
    let doneByDay = groupByEq (fromJust . taskDoneDay timeZone . fst . snd) done
    ((_, taskWidget), enctype) <- generateFormPost taskForm
    defaultLayout $ do
        setTitle "tasks"
        addWidget $(widgetFile "tasks")

  userTasks userId = selectList [TaskUser ==. userId] [Asc TaskOrder, Desc TaskDoneAt] -- must specify sorts backwards...
  taskEstimates taskId = selectList [EstimateTask ==. taskId] []
  taskTr (taskId, (task, estimates)) = $(widgetFile "tasks/task-tr")
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
postTasksR = maybeAuthId >>= postTasksR' where
  postTasksR' :: Maybe UserId -> Handler RepHtml
  postTasksR' Nothing = redirectTemporary RootR
  postTasksR' (Just userId) = do
    ((result, taskWidget), _) <- runFormPost taskForm
    case result of
      FormSuccess task -> do
        runDB $ createTaskAtBottom userId task
        redirectTemporary TasksR
      _ -> undefined -- TODO


postCompleteTaskR :: TaskId -> Handler RepHtml
postCompleteTaskR taskId = do
  now <- liftIO getCurrentTime
  updateAndRedirectR TasksR [TaskDoneAt =. Just now] taskId

postRestartTaskR :: TaskId -> Handler RepHtml
postRestartTaskR = updateAndRedirectR TasksR [TaskDoneAt =. Nothing]

updateAndRedirectR :: HasReps a => YesodoroRoute -> [Update Task] -> TaskId -> Handler a
updateAndRedirectR route updates taskId = maybeAuthId >>= updateAndRedirectR' route updates taskId where
  updateAndRedirectR' :: HasReps a => YesodoroRoute -> [Update Task] -> TaskId -> Maybe UserId -> Handler a
  updateAndRedirectR' _ _ _ Nothing = redirectTemporary RootR
  updateAndRedirectR' route updates taskId (Just userId) = do
    runDB $ updateWhere [TaskId ==. taskId, TaskUser ==. userId] updates
    redirectTemporary route

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
postDeleteTaskR taskId = do
  runDB $ deleteWhere [EstimateTask ==. taskId]
  runDB $ delete taskId
  redirectTemporary TasksR

postTaskAddPomoR :: TaskId -> Handler RepHtml
postTaskAddPomoR = updateAndRedirectR TasksR [TaskPomos +=. 1]

postTaskAddEstimateR :: TaskId -> Handler RepHtml
postTaskAddEstimateR taskId = maybeAuthId >>= postTaskAddEstimateR' taskId where
  postTaskAddEstimateR' :: TaskId -> Maybe UserId -> Handler RepHtml
  postTaskAddEstimateR' taskId (Just userId) = do
    authzedTaskId <- checkAuthz userId taskId
    pomosParam <- lookupPostParam "pomos"
    let pomos = do
        param <- maybeToEither "no pomos" $ pomosParam
        (num, _) <- decimal param
        return num
    postTaskAddEstimateR'' authzedTaskId pomos
  postTaskAddEstimateR' _ Nothing = redirectTemporary RootR

  checkAuthz :: UserId -> TaskId -> Handler (Maybe TaskId)
  checkAuthz userId taskId = do
    maybeIdAndTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
    return $ fmap fst maybeIdAndTask

  postTaskAddEstimateR'' :: Maybe TaskId -> Either String Int -> Handler RepHtml
  postTaskAddEstimateR'' (Just taskId) (Right pomos) = do
    runDB $ insert $ Estimate taskId pomos
    redirectTemporary TasksR
  postTaskAddEstimateR'' authzedTask pomosOrError = do
    $(logInfo) $ pack $ "task: " ++ show authzedTask ++ "; pomos: " ++ show pomosOrError
    redirectTemporary TasksR

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
reorderTaskR direction taskId = maybeAuthId >>= reorderTaskR' direction taskId where
  reorderTaskR' :: Direction -> TaskId -> Maybe UserId -> Handler RepHtml
  reorderTaskR' _ _ Nothing = redirectTemporary RootR
  reorderTaskR' direction taskId (Just userId) = do
    runDB $ reorderTask direction [TaskUser ==. userId, TaskId ==. taskId]
    redirectTemporary TasksR
