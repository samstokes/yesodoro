{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Control.Applicative
import Data.Text (Text)
import Database.Persist.GenericSql.Raw (SqlPersist)
import Yesod.Auth (maybeAuthId)
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
    ((_, taskWidget), enctype) <- generateFormPost $ taskForm userId
    defaultLayout $ do
        setTitle "tasks"
        addWidget $(widgetFile "tasks")

  userTasks userId = selectList [TaskUser ==. userId] [Asc TaskDone]


oneButton :: Text -> YesodoroRoute -> Widget
oneButton label route = [whamlet|
  <form method=POST action=@{route}
    <input type=submit value=#{label}
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

setTaskDonenessButton :: (TaskId, Task) -> Widget
setTaskDonenessButton (taskId, task) = oneButton action (route taskId)
  where action = taskActionName task
        route | taskDone task = RestartTaskR
              | otherwise     = CompleteTaskR

postDeleteTaskR :: TaskId -> Handler RepHtml
postDeleteTaskR taskId = do
  runDB $ delete taskId
  redirectTemporary TasksR
