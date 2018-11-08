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
getHomeR = redirect $ MyprojectR MyprojectHomeR

getMyprojectHomeR :: Handler Html
getMyprojectHomeR = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{MyprojectR $ HomeDataR}")
                     \ })
                   |]

getHomeDataR :: Handler Value
getHomeDataR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavHome
  let pages = defaultDataPages
        { jDataPageHome = Just $ JDataPageHome { jDataPageHomeContent = "todo" }
        }
  msgHome <- localizedMsg MsgGlobalHome
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ MyprojectR HomeDataR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ MyprojectR MyprojectHomeR
      , jDataHistoryStateTitle = msgHome
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems =
      [ JDataBreadcrumbItem
        { jDataBreadcrumbItemLabel = msgHome
        , jDataBreadcrumbItemDataUrl = currentDataUrl }
      ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ MyprojectR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ MyprojectR $ LanguageEnR currentDataUrl
    }

getRiotTagsR :: Handler Html
getRiotTagsR = withUrlRenderer $(hamletFile "templates/riot_tags.hamlet")

postLanguageDeR :: Text -> Handler Value
postLanguageDeR dataUrlStr = do
  setLanguage "de"
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = dataUrlStr }

postLanguageEnR :: Text -> Handler Value
postLanguageEnR dataUrlStr = do
  setLanguage "en-US"
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = dataUrlStr }
