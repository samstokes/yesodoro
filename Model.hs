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

newTask :: UserId -> NewTask -> Task
newTask uid (NewTask title) = Task uid title 0 False

taskState :: Task -> TaskState
taskState task = if taskDone task then "done" else "pending"

taskActionName :: Task -> Text
taskActionName task | taskDone task = "Restart"
                    | otherwise     = "Complete"

estimateOptions :: [Int]
estimateOptions = [2 ^ x | x <- [0 .. 4]]
