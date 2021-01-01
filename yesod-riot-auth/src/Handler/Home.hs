{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Handler.Common
import Import
import Text.Hamlet (hamletFile)

getHomeR :: Handler Html
getHomeR = redirect $ BackendR BackendHomeR

getBackendHomeR :: Handler Html
getBackendHomeR = do
  let route = BackendR HomeDataR
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
  let pages =
        defaultDataPages
          { jDataPageHome = Just $ JDataPageHome {jDataPageHomeContent = "todo"}
          }
  msgHome <- localizedMsg MsgGlobalHome
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ BackendR HomeDataR
  returnJson
    JData
      { jDataAppName = appName,
        jDataUserIdent = userIdent user,
        jDataMainNavItems = mainNavItems,
        jDataSubNavItems = [],
        jDataPages = pages,
        jDataHistoryState =
          Just
            JDataHistoryState
              { jDataHistoryStateUrl = urlRenderer $ BackendR BackendHomeR,
                jDataHistoryStateTitle = msgHome
              },
        jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName,
        jDataCsrfParamName = defaultCsrfParamName,
        jDataCsrfToken = reqToken req,
        jDataBreadcrumbItems =
          [ JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgHome,
                jDataBreadcrumbItemDataUrl = currentDataUrl
              }
          ],
        jDataCurrentLanguage = currentLanguage,
        jDataTranslation = translation,
        jDataLanguageDeUrl = urlRenderer $ BackendR $ LanguageDeR currentDataUrl,
        jDataLanguageEnUrl = urlRenderer $ BackendR $ LanguageEnR currentDataUrl
      }

getRiotBodyTagR :: Handler Html
getRiotBodyTagR = withUrlRenderer $(hamletFile "templates/riot/body_tag.hamlet")

getRiotNavTagR :: Handler Html
getRiotNavTagR = withUrlRenderer $(hamletFile "templates/riot/nav_tag.hamlet")

getRiotRawTagR :: Handler Html
getRiotRawTagR = withUrlRenderer $(hamletFile "templates/riot/raw_tag.hamlet")

getRiotPaginationTagR :: Handler Html
getRiotPaginationTagR = withUrlRenderer $(hamletFile "templates/riot/pagination_tag.hamlet")

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
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = dataUrlStr}

postLanguageEnR :: Text -> Handler Value
postLanguageEnR dataUrlStr = do
  setLanguage "en-US"
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = dataUrlStr}
