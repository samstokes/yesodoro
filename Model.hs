{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs, OverloadedStrings #-}
module Model where

import Yesod
import Data.String (IsString)
import Data.Text (Text)
import Text.Blaze (ToHtml)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")


newtype TaskState = TaskState Text
  deriving (ToHtml, IsString)


data NewTask = NewTask { newTaskTitle :: Text } deriving (Show)

newTask :: UserId -> Int -> NewTask -> Task
newTask uid order (NewTask title) = Task uid title 0 False order


createTaskAtBottom userId task = do
  maybeLastTask <- runDB $ selectFirst [TaskUser ==. userId] [Desc TaskOrder]
  let lastOrder = maybe 0 (taskOrder . snd) maybeLastTask
  runDB $ insert $ newTask userId (succ lastOrder) task

taskState :: Task -> TaskState
taskState task = if taskDone task then "done" else "pending"

taskActionName :: Task -> Text
taskActionName task | taskDone task = "Restart"
                    | otherwise     = "Complete"

estimateOptions :: [Int]
estimateOptions = [2 ^ x | x <- [0 .. 4]]
