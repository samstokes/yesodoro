{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withYesodoro
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Settings.StaticFiles (static)
import Yesod.Auth
import Yesod.Logger (makeLogger, flushLogger, Logger, logString, logLazyText)
import Database.Persist.GenericSql
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)
import Network.Wai.Middleware.Debug (debugHandle)

#ifndef WINDOWS
import qualified System.Posix.Signals as Signal
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
#endif

-- Import all relevant handler modules here.
import Handler.Root

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Yesodoro" resourcesYesodoro

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withYesodoro :: AppConfig -> Logger -> (Application -> IO a) -> IO ()
withYesodoro conf logger f = do
    s <- static Settings.staticDir
    Settings.withConnectionPool conf $ \p -> do
        runConnectionPool (runMigration migrateAll) p
        let h = Yesodoro conf logger s p
#ifdef WINDOWS
        toWaiApp h >>= f >> return ()
#else
        tid <- forkIO $ toWaiApp h >>= f >> return ()
        flag <- newEmptyMVar
        _ <- Signal.installHandler Signal.sigINT (Signal.CatchOnce $ do
            putStrLn "Caught an interrupt"
            killThread tid
            putMVar flag ()) Nothing
        takeMVar flag
#endif

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort =
    toDyn go
  where
    go :: ((Int, Application) -> IO ()) -> IO ()
    go f = do
        conf <- Settings.loadConfig Settings.Development
        let port = appPort conf
        logger <- makeLogger
        logString logger $ "Devel application launched, listening on port " ++ show port
        withYesodoro conf logger $ \app -> f (port, debugHandle (logHandle logger) app)
        flushLogger logger
      where
        logHandle logger msg = logLazyText logger msg >> flushLogger logger