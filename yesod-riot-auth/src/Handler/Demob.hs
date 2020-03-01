{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Demob where

import Handler.Common
import Import
import Text.Hamlet (hamletFile)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI
import qualified Database.Esqueleto as E

-------------------------------------------------------
-- list
-------------------------------------------------------

getDemobListR :: Handler Html
getDemobListR = do
  let route = MyprojectR DemobListDataR
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getDemobListDataR :: Handler Value
getDemobListDataR = demobListPageNumDataR 1

postDemobListPageNumDataR :: Int -> Handler Value
postDemobListPageNumDataR pageNum = do
  urlRenderer <- getUrlRender
  returnJson $
    VFormSubmitSuccess
    { fsSuccessDataJsonUrl = urlRenderer $ MyprojectR $ DemobListPageNumDataR pageNum }

getDemobListPageNumDataR :: Int -> Handler Value
getDemobListPageNumDataR = demobListPageNumDataR

demobListPageNumDataR :: Int -> Handler Value
demobListPageNumDataR pageNum = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavDemob
  (jDataDemobs, jDataPaginationItems) <- demobListJDatas pageNum
  let pages =
        defaultDataPages
        { jDataPageDemobList =
            Just $ JDataPageDemobList
            { jDataPageDemobListDemobs = jDataDemobs
            , jDataPageDemobListAddFormUrl = urlRenderer $ MyprojectR AddDemobFormR
            , jDataPageDemobListPaginationItems = jDataPaginationItems
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgDemobs <- localizedMsg MsgDemobDemobs
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ MyprojectR DemobListDataR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ MyprojectR DemobListR
      , jDataHistoryStateTitle = msgDemobs
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ MyprojectR HomeDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgDemobs
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ MyprojectR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ MyprojectR $ LanguageEnR currentDataUrl
    }

demobListJDatas :: Int -> Handler ([JDataDemob], Maybe [JDataPaginationItem])
demobListJDatas pageNum = do
  urlRenderer <- getUrlRender
  rowCount <- runDB $ count ([] :: [Filter Demob])
  paginationJDatas <- getPaginationJDatas rowCount demobListPageSize pageNum 11 (MyprojectR . DemobListPageNumDataR)
  demobEnts <- runDB loadDemobTuples
  let demobJDatas =
        map (\demobEnt@(Entity demobId _) ->
               JDataDemob
               { jDataDemobEnt = demobEnt
               , jDataDemobDetailUrl = urlRenderer $ MyprojectR $ DemobDetailR demobId
               , jDataDemobDetailDataUrl = urlRenderer $ MyprojectR $ DemobDetailDataR demobId
               , jDataDemobDeleteFormUrl = urlRenderer $ MyprojectR $ DeleteDemobFormR demobId
               }
            ) demobEnts
  return (demobJDatas, paginationJDatas)
  where
    loadDemobTuples :: YesodDB App [(Entity Demob)]
    loadDemobTuples = do
      let pageSize = fromIntegral demobListPageSize
      E.select $ E.from $ \(da) -> do
        E.orderBy [ E.desc (da E.^. DemobId) ]
        E.offset ((fromIntegral pageNum - 1) * pageSize)
        E.limit pageSize
        return (da)

demobListPageSize :: Int
demobListPageSize = 5

-------------------------------------------------------
-- detail
-------------------------------------------------------

getDemobDetailR :: DemobId -> Handler Html
getDemobDetailR demobId = do
  let route = MyprojectR $ DemobDetailDataR demobId
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getDemobDetailDataR :: DemobId -> Handler Value
getDemobDetailDataR demobId = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  mainNavItems <- mainNavData user MainNavDemob
  demob <- runDB $ get404 demobId
  jDataDemocs <- democJDatas demobId
  urlRenderer <- getUrlRender
  let pages =
        defaultDataPages
        { jDataPageDemobDetail =
            Just $ JDataPageDemobDetail
            { jDataPageDemobDetailDemobEnt = Entity demobId demob
            , jDataPageDemobDetailDemocs = jDataDemocs
            , jDataPageDemobDetailDemobEditFormUrl = urlRenderer $ MyprojectR $ EditDemobFormR demobId
            , jDataPageDemobDetailDemocAddFormUrl = urlRenderer $ MyprojectR $ AddDemocFormR demobId
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgDemobs <- localizedMsg MsgDemobDemobs
  msgDemob <- localizedMsg MsgDemobDemob
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ MyprojectR $ DemobDetailDataR demobId
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ MyprojectR $ DemobDetailR demobId
      , jDataHistoryStateTitle = msgDemob
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ MyprojectR HomeDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgDemobs
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ MyprojectR DemobListDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = demobMyattr demob
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ MyprojectR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ MyprojectR $ LanguageEnR currentDataUrl
    }

democJDatas :: DemobId -> Handler [JDataDemoc]
democJDatas demobId = do
  urlRenderer <- getUrlRender
  democTuples <- runDB loadDemocTuples
  return $ map
    (\(democEnt@(Entity democId _)) ->
       JDataDemoc
       { jDataDemocEnt = democEnt
       , jDataDemocEditFormUrl = urlRenderer $ MyprojectR $ EditDemocFormR democId
       , jDataDemocDeleteFormUrl = urlRenderer $ MyprojectR $ DeleteDemocFormR democId
       })
    democTuples
  where
    loadDemocTuples :: YesodDB App [(Entity Democ)]
    loadDemocTuples =
      E.select $ E.from $ \(dc) -> do
        E.orderBy [ E.desc (dc E.^. DemocId) ]
        E.where_ (dc E.^. DemocDemobId E.==. E.val demobId)
        return (dc)

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddDemob = VAddDemob
  { vAddDemobMyattr :: Text
  }
-- gen data add - end

-- gen get add form - start
getAddDemobFormR :: Handler Html
getAddDemobFormR = do
  (formWidget, _) <- generateFormPost $ vAddDemobForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgDemobAddDemob}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{MyprojectR AddDemobR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add - start
postAddDemobR :: Handler Value
postAddDemobR = do
  ((result, formWidget), _) <- runFormPost $ vAddDemobForm Nothing
  case result of
    FormSuccess vAddDemob -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let demob = Demob
            {
            demobMyattr = vAddDemobMyattr vAddDemob
            , demobVersion = 1
            , demobCreatedAt = curTime
            , demobCreatedBy = userIdent authUser
            , demobUpdatedAt = curTime
            , demobUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert demob
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ MyprojectR DemobListDataR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add - end

-- gen add form - start
vAddDemobForm :: Maybe Demob -> Html -> MForm Handler (FormResult VAddDemob, Widget)
vAddDemobForm maybeDemob extra = do
  (myattrResult, myattrView) <- mreq textField
    myattrFs
    (demobMyattr <$> maybeDemob)
  let vAddDemobResult = VAddDemob <$> myattrResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors myattrView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors myattrView:.uk-text-danger for=#{fvId myattrView}>#{fvLabel myattrView}
      <div .uk-form-controls>
        ^{fvInput myattrView}
        $maybe err <- fvErrors myattrView
          &nbsp;#{err}
    |]
  return (vAddDemobResult, formWidget)
  where
    myattrFs :: FieldSettings App
    myattrFs = FieldSettings
      { fsLabel = SomeMessage MsgDemobMyattr
      , fsTooltip = Nothing
      , fsId = Just "myattr"
      , fsName = Just "myattr"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditDemob = VEditDemob
  { vEditDemobMyattr :: Text
  , vEditDemobVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditDemobFormR :: DemobId -> Handler Html
getEditDemobFormR demobId = do
  demob <- runDB $ get404 demobId
  (formWidget, _) <- generateFormPost $ vEditDemobForm (Just demob)
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgDemobEditDemob}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{MyprojectR $ EditDemobR demobId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit - start
postEditDemobR :: DemobId -> Handler Value
postEditDemobR demobId = do
  ((result, formWidget), _) <- runFormPost $ vEditDemobForm Nothing
  case result of
    FormSuccess vEditDemob -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            DemobMyattr =. vEditDemobMyattr vEditDemob
            , DemobVersion =. vEditDemobVersion vEditDemob + 1
            , DemobUpdatedAt =. curTime
            , DemobUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ DemobId ==. demobId
                               , DemobVersion ==. vEditDemobVersion vEditDemob
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ MyprojectR $ DemobDetailDataR demobId }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ MyprojectR $ DemobDetailDataR demobId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit - end

-- gen edit form - start
vEditDemobForm :: Maybe Demob -> Html -> MForm Handler (FormResult VEditDemob, Widget)
vEditDemobForm maybeDemob extra = do
  (myattrResult, myattrView) <- mreq textField
    myattrFs
    (demobMyattr <$> maybeDemob)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (demobVersion <$> maybeDemob)
  let vEditDemobResult = VEditDemob <$> myattrResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors myattrView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors myattrView:.uk-text-danger for=#{fvId myattrView}>#{fvLabel myattrView}
      <div .uk-form-controls>
        ^{fvInput myattrView}
        $maybe err <- fvErrors myattrView
          &nbsp;#{err}
    |]
  return (vEditDemobResult, formWidget)
  where
    myattrFs :: FieldSettings App
    myattrFs = FieldSettings
      { fsLabel = SomeMessage MsgDemobMyattr
      , fsTooltip = Nothing
      , fsId = Just "myattr"
      , fsName = Just "myattr"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }
-- gen edit form - end

-------------------------------------------------------
-- delete
-------------------------------------------------------

-- gen get delete form - start
getDeleteDemobFormR :: DemobId -> Handler Html
getDeleteDemobFormR demobId = do
  (formWidget, _) <- generateFormPost $ vDeleteDemobForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgDemobDeleteDemob}
      <form #modal-form .uk-form-horizontal method=post action=@{MyprojectR $ DeleteDemobR demobId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete - start
postDeleteDemobR :: DemobId -> Handler Value
postDeleteDemobR demobId = do
  runDB $ delete demobId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ MyprojectR DemobListDataR }
-- gen post delete - end

-- gen delete form - start
vDeleteDemobForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteDemobForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
