{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Control.Applicative
import Data.Text (Text)
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
getRootR = do
    mu <- maybeAuth
    let uid = fmap fst mu
    tasks <- maybe (return []) (runDB . userTasks) uid
    ((_, taskWidget), _) <- maybe (return ((undefined, [whamlet|""|]), undefined)) (generateFormPost . taskForm) uid
    defaultLayout $ do
        setTitle "yesodoro"
        addWidget $(widgetFile "homepage")
  where
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
postTasksR = do
  uid <- maybeAuthId
  ((result, taskWidget), _) <- maybe (return ((FormMissing, [whamlet|""|]), undefined)) (runFormPost . taskForm) uid
  case result of
    FormSuccess task -> do
      runDB $ insert task
      redirectTemporary RootR
    _ -> undefined


postCompleteTaskR :: TaskId -> Handler RepHtml
postCompleteTaskR = setTaskDoneness True

postRestartTaskR :: TaskId -> Handler RepHtml
postRestartTaskR = setTaskDoneness False

setTaskDoneness :: Bool -> TaskId -> Handler RepHtml
setTaskDoneness done taskId = do
  runDB $ update taskId [TaskDone =. done]
  redirect RedirectTemporary RootR

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
  redirect RedirectTemporary RootR
