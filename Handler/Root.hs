{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Data.Text.Read
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
    estimates <- mapM (runDB . taskEstimates . fst) tasks
    let tasksEstimates = M.fromList $ zip (map fst tasks) estimates
    ((_, taskWidget), enctype) <- generateFormPost $ taskForm userId
    defaultLayout $ do
        setTitle "tasks"
        addWidget $(widgetFile "tasks")

  userTasks userId = selectList [TaskUser ==. userId] [Asc TaskDone]
  taskEstimates taskId = selectList [EstimateTask ==. taskId] []
  taskTr (taskId, task) estimates = $(widgetFile "tasks/task-tr")


oneButton :: Text -> YesodoroRoute -> Widget
oneButton label route = [whamlet|
  <form method=POST action=@{route}
    <button>#{label}
|]

taskForm :: UserId -> Html -> Form Yesodoro Yesodoro (FormResult Task, Widget)
taskForm uid = renderDivs $ Task uid
  <$> areq textField "Title" Nothing
  <*> pure 0
  <*> pure False

postTasksR :: Handler RepHtml
postTasksR = maybeAuthId >>= postTasksR' where
  postTasksR' :: Maybe UserId -> Handler RepHtml
  postTasksR' Nothing = redirectTemporary RootR
  postTasksR' (Just userId) = do
    ((result, taskWidget), _) <- runFormPost $ taskForm userId
    case result of
      FormSuccess task -> do
        runDB $ insert task
        redirectTemporary TasksR
      _ -> undefined -- TODO


postCompleteTaskR :: TaskId -> Handler RepHtml
postCompleteTaskR = updateAndRedirect TasksR [TaskDone =. True]

postRestartTaskR :: TaskId -> Handler RepHtml
postRestartTaskR = updateAndRedirect TasksR [TaskDone =. False]

updateAndRedirect :: (PersistEntity val, HasReps a) => YesodoroRoute -> [Update val] -> Key SqlPersist val -> Handler a
updateAndRedirect route updates fieldId = do
  runDB $ update fieldId updates
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
postTaskAddPomoR = updateAndRedirect TasksR [TaskPomos +=. 1]

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
