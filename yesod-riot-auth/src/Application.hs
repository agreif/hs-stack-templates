{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( getApplicationDev,
    appMain,
    develMain,
    genRiotFilesMain,
    makeFoundation,
    makeLogWare,

    -- * for DevelMain
    getApplicationRepl,
    shutdownApp,

    -- * for GHCI
    handler,
    db,
  )
where

import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Postgresql
  ( createPostgresqlPool,
    pgConnStr,
    pgPoolSize,
    runSqlPool,
  )
-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Handler.Admin
import Handler.Common
import Handler.Config
import Handler.Demoa
import Handler.Demob
import Handler.Democ
import Handler.Home
import Handler.InitDb
import Handler.MyProfile
import Handler.TestMail
import Handler.User
import Import
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    defaultShouldDisplayException,
    getPort,
    runSettings,
    setHost,
    setOnException,
    setPort,
    setServerName,
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (Logger),
    IPAddrSource (..),
    OutputFormat (..),
    destination,
    mkRequestLogger,
    outputFormat,
  )
import System.Directory (createDirectoryIfMissing)
import System.Log.FastLogger
  ( defaultBufSize,
    newStdoutLoggerSet,
    toLogStr,
  )
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  -- Some basic initializations: HTTP connection manager, logger, and static
  -- subsite.
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
      (appStaticDir appSettings)
  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool = App {..}
      -- The App {..} syntax is an example of record wild cards. For more
      -- information, see:
      -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger
  -- Create the database connection pool
  pool <-
    flip runLoggingT logFunc $
      createPostgresqlPool
        (pgConnStr $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)
  -- Perform database migration using our application's logging settings.
  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
  -- Return the foundation
  return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
      { outputFormat =
          if appDetailedRequestLogging $ appSettings foundation
            then Detailed True
            else
              Apache
                ( if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket
                ),
        destination = Logger $ loggerSet $ appLogger foundation
      }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setServerName ""
    $ setOnException
      ( \_req e ->
          when (defaultShouldDisplayException e) $
            messageLoggerSource
              foundation
              (appLogger foundation)
              $(qLocation >>= liftLoc)
              "yesod"
              LevelError
              (toLogStr $ "Exception from Warp: " ++ show e)
      )
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  -- Get the settings from all relevant sources
  settings <-
    loadYamlSettingsArgs
      -- fall back to compile-time values, set to [] to require values at runtime
      [configSettingsYmlValue]
      -- allow environment variables to override
      useEnv
  -- Generate the foundation from the settings
  foundation <- makeFoundation settings
  -- Generate a WAI Application from the foundation
  app <- makeApplication foundation
  -- Run the application with Warp
  runSettings (warpSettings foundation) app

genRiotFilesMain :: IO ()
genRiotFilesMain = do
  settings <-
    loadYamlSettingsArgs
      [configSettingsYmlValue]
      useEnv
  let dir = riotDir settings
  createDirectoryIfMissing True dir
  writeRiotFile dir getRiotBodyTagR "body_tag.riot"
  writeRiotFile dir getRiotNavTagR "nav_tag.riot"
  writeRiotFile dir getRiotRawTagR "raw_tag.riot"
  writeRiotFile dir getRiotPaginationTagR "pagination_tag.riot"
  writeRiotFile dir getRiotHomePageTagR "home_page_tag.riot"
  writeRiotFile dir getRiotAdminPageTagR "admin_page_tag.riot"
  writeRiotFile dir getRiotDemoaListPageTagR "demoa_list_page_tag.riot"
  writeRiotFile dir getRiotDemobListPageTagR "demob_list_page_tag.riot"
  writeRiotFile dir getRiotDemobDetailPageTagR "demob_detail_page_tag.riot"
  where
    riotDir :: AppSettings -> FilePath
    riotDir settings = appStaticDir settings ++ "/js/riot/"
    writeRiotFile :: FilePath -> Handler Html -> String -> IO ()
    writeRiotFile dir handlerFunc jsFilename = do
      print $ "gen riot file: " ++ dir ++ jsFilename
      handler $ $logInfo $ "gen riot file: " ++ pack jsFilename
      h <- handler handlerFunc
      withSinkFileCautious (dir ++ jsFilename) $ \sink ->
        runConduit $ sourceLazy (renderHtml h) .| sink

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app1 <- makeApplication foundation
  return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
