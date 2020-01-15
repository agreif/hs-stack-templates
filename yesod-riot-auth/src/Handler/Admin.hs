{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Admin where

import Handler.Common
import Import
import Text.Hamlet (hamletFile)
import qualified Database.Esqueleto as E
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

getAdminHomeR :: Handler Html
getAdminHomeR = do
  let route = AdminR AdminDataR
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getAdminDataR :: Handler Value
getAdminDataR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavAdmin
  jDataUsers <- userListJDataEnts
  jDataConfigs <- configListJDataEnts
  let pages =
        defaultDataPages
        { jDataPageAdmin =
            Just $ JDataPageAdmin
            { jDataPageAdminUsers = jDataUsers
            , jDataPageAdminConfigs = jDataConfigs
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgAdmin <- localizedMsg MsgGlobalAdmin
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ AdminR AdminDataR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ AdminR AdminHomeR
      , jDataHistoryStateTitle = msgAdmin
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ MyprojectR HomeDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgAdmin
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ MyprojectR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ MyprojectR $ LanguageEnR currentDataUrl
    }

userListJDataEnts :: Handler [JDataUser]
userListJDataEnts = do
  urlRenderer <- getUrlRender
  userTuples <- runDB loadUserListTuples
  let jUserList = map (\userEnt@(Entity userId _) ->
                           JDataUser
                           { jDataUserEnt = userEnt
                           , jDataUserEditFormUrl = urlRenderer $ AdminR $ EditUserFormR userId
                           , jDataUserDeleteFormUrl = urlRenderer $ AdminR $ DeleteUserFormR userId
                           }
                        ) userTuples
  return jUserList

loadUserListTuples :: YesodDB App [Entity User]
loadUserListTuples =
  E.select $ E.from $ \user -> do
    E.orderBy [E.asc (user E.^. UserId)]
    return user

configListJDataEnts :: Handler [JDataConfig]
configListJDataEnts = do
  urlRenderer <- getUrlRender
  configTuples <- runDB loadConfigListTuples
  let jConfigList = map (\configEnt@(Entity configId _) ->
                           JDataConfig
                           { jDataConfigEnt = configEnt
                           , jDataConfigEditFormUrl = urlRenderer $ AdminR $ EditConfigFormR configId
                           }
                        ) configTuples
  return jConfigList

loadConfigListTuples :: YesodDB App [Entity Config]
loadConfigListTuples =
  E.select $ E.from $ \config -> do
    E.orderBy [E.asc (config E.^. ConfigId)]
    return config
