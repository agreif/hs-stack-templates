{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.InitDb where

import Import
import Handler.Common

getInitDbR :: Text -> Handler ()
getInitDbR adminEmail = do
  curTime <- liftIO getCurrentTime
  runDB $ do
    userCount <- count ([] :: [Filter User])
    when (userCount == 0)
      ( do
        (adminPasswd, adminPasswdHash) <- liftIO $ generatePassword 32
        _ <- insert $ User { userIdent = "admin"
                           , userPassword = Just adminPasswdHash
                           , userEmail = adminEmail
                           , userIsAdmin = True
                           , userVersion = 1
                           , userCreatedAt = curTime
                           , userCreatedBy = dbSystemUser
                           , userUpdatedAt = curTime
                           , userUpdatedBy = dbSystemUser
                           }
        $(logError) $ "###############################"
        $(logError) $ "admin login:         admin"
        $(logError) $ "admin password:      " ++ adminPasswd
        $(logError) $ "admin password hash: " ++ adminPasswdHash
        $(logError) $ "admin email:         " ++ adminEmail
        $(logError) $ "###############################"
      )

    maybeConfigAppName <- selectFirst [ConfigCode ==. "app_name"] []
    case maybeConfigAppName of
      Nothing -> do
        let appName = "hs-yesod-riot-auth"
        _ <- insert $ Config { configCode = "app_name"
                             , configStringValue = Just appName
                             , configIntValue = Nothing
                             , configDoubleValue = Nothing
                             , configBoolValue = False
                             , configVersion = 1
                             , configCreatedAt = curTime
                             , configCreatedBy = dbSystemUser
                             , configUpdatedAt = curTime
                             , configUpdatedBy = dbSystemUser }
        return ()
      Just _  -> return ()

    maybeConfigEmailFrom <- selectFirst [ConfigCode ==. "email_from"] []
    case maybeConfigEmailFrom of
      Nothing -> do
        _ <- insert $ Config { configCode = "email_from"
                             , configStringValue = Just "info@example.com"
                             , configIntValue = Nothing
                             , configDoubleValue = Nothing
                             , configBoolValue = False
                             , configVersion = 1
                             , configCreatedAt = curTime
                             , configCreatedBy = dbSystemUser
                             , configUpdatedAt = curTime
                             , configUpdatedBy = dbSystemUser }
        return ()
      Just _ -> return ()
  redirect HomeR
