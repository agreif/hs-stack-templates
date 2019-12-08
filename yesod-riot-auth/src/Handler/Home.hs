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
getMyprojectHomeR = do
  let route = MyprojectR HomeDataR
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

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

getRiotBodyTagR :: Handler Html
getRiotBodyTagR = withUrlRenderer $(hamletFile "templates/riot/body_tag.hamlet")

getRiotNavTagR :: Handler Html
getRiotNavTagR = withUrlRenderer $(hamletFile "templates/riot/nav_tag.hamlet")

getRiotHomePageTagR :: Handler Html
getRiotHomePageTagR = withUrlRenderer $(hamletFile "templates/riot/home_page_tag.hamlet")

getRiotAdminPageTagR :: Handler Html
getRiotAdminPageTagR = withUrlRenderer $(hamletFile "templates/riot/admin_page_tag.hamlet")

getRiotDemoaListPageTagR :: Handler Html
getRiotDemoaListPageTagR = withUrlRenderer $(hamletFile "templates/riot/demoa_list_page_tag.hamlet")

getRiotDemobListPageTagR :: Handler Html
getRiotDemobListPageTagR = withUrlRenderer $(hamletFile "templates/riot/demob_list_page_tag.hamlet")

getRiotDemobDetailPageTagR :: Handler Html
getRiotDemobDetailPageTagR = withUrlRenderer $(hamletFile "templates/riot/demob_detail_page_tag.hamlet")

postLanguageDeR :: Text -> Handler Value
postLanguageDeR dataUrlStr = do
  setLanguage "de"
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = dataUrlStr }

postLanguageEnR :: Text -> Handler Value
postLanguageEnR dataUrlStr = do
  setLanguage "en-US"
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = dataUrlStr }
