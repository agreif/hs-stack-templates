{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import Handler.Common
import Import
import Text.Hamlet (hamletFile)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

getHomeR :: Handler Html
getHomeR = redirect $ SimulationR SimulationHomeR

getSimulationHomeR :: Handler Html
getSimulationHomeR = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{SimulationR $ HomePageDataJsonR}")
                     \ })
                   |]

getHomePageDataJsonR :: Handler Value
getHomePageDataJsonR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData MainNavHome
  let pages = defaultDataPages
        { jDataPageHome = Just $ JDataPageHome { jDataPageHomeContent = "todo" }
        }
  msgHome <- localizedMsg MsgGlobalHome
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ SimulationR SimulationHomeR
      , jDataHistoryStateTitle = msgHome
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ SimulationR HomePageDataJsonR }
                             ]
    }

getRiotTagsR :: Handler Html
getRiotTagsR = withUrlRenderer $(hamletFile "templates/riot_tags.hamlet")
