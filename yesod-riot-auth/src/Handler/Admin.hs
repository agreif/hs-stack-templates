{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Admin where

import Handler.Common
import Import
import qualified Database.Esqueleto as E
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

getAdminR :: Handler Html
getAdminR = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{SimulationR $ AdminPageDataJsonR}")
                     \ })
                   |]

getAdminPageDataJsonR :: Handler Value
getAdminPageDataJsonR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData MainNavAdmin
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
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ SimulationR AdminR
      , jDataHistoryStateTitle = msgAdmin
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ SimulationR HomePageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgAdmin
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ SimulationR AdminPageDataJsonR }
                             ]
    }

userListJDataEnts :: Handler [JDataUser]
userListJDataEnts = do
  urlRenderer <- getUrlRender
  userTuples <- runDB loadUserListTuples
  let jUserList = map (\(userEnt@(Entity userId _)) ->
                           JDataUser
                           { jDataUserEnt = userEnt
                           , jDataUserEditFormUrl = urlRenderer $ SimulationR $ EditUserFormR userId
                           , jDataUserDeleteFormUrl = urlRenderer $ SimulationR $ DeleteUserFormR userId
                           }
                        ) userTuples
  return jUserList

loadUserListTuples :: YesodDB App [(Entity User)]
loadUserListTuples = do
  tuples <- E.select $ E.from $ \(user) -> do
    E.orderBy [E.asc (user E.^. UserId)]
    return (user)
  return tuples

configListJDataEnts :: Handler [JDataConfig]
configListJDataEnts = do
  urlRenderer <- getUrlRender
  configTuples <- runDB loadConfigListTuples
  let jConfigList = map (\(configEnt@(Entity configId _)) ->
                           JDataConfig
                           { jDataConfigEnt = configEnt
                           , jDataConfigEditFormUrl = urlRenderer $ SimulationR $ EditConfigFormR configId
                           }
                        ) configTuples
  return jConfigList

loadConfigListTuples :: YesodDB App [(Entity Config)]
loadConfigListTuples = do
  tuples <- E.select $ E.from $ \(config) -> do
    E.orderBy [E.asc (config E.^. ConfigId)]
    return (config)
  return tuples
