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
                     \   bodyTag.refreshData("@{MyprojectR $ AdminPageDataJsonR}")
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
      { jDataHistoryStateUrl = urlRenderer $ MyprojectR AdminR
      , jDataHistoryStateTitle = msgAdmin
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ MyprojectR HomePageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgAdmin
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ MyprojectR AdminPageDataJsonR }
                             ]
    }

userListJDataEnts :: Handler [JDataUser]
userListJDataEnts = do
  urlRenderer <- getUrlRender
  userTuples <- runDB loadUserListTuples
  let jUserList = map (\(userEnt@(Entity userId _)) ->
                           JDataUser
                           { jDataUserEnt = userEnt
                           , jDataUserEditFormUrl = urlRenderer $ MyprojectR $ EditUserFormR userId
                           , jDataUserDeleteFormUrl = urlRenderer $ MyprojectR $ DeleteUserFormR userId
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
                           , jDataConfigEditFormUrl = urlRenderer $ MyprojectR $ EditConfigFormR configId
                           }
                        ) configTuples
  return jConfigList

loadConfigListTuples :: YesodDB App [(Entity Config)]
loadConfigListTuples = do
  tuples <- E.select $ E.from $ \(config) -> do
    E.orderBy [E.asc (config E.^. ConfigId)]
    return (config)
  return tuples
