{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Demoa where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto as E
import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Text.Hamlet (hamletFile)
import Prelude (read)

-------------------------------------------------------
-- list
-------------------------------------------------------

getDemoaListR :: Handler Html
getDemoaListR = do
  let route = MyprojectR DemoaListDataR
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getDemoaListDataR :: Handler Value
getDemoaListDataR = demoaListPageNumDataR 1

sortColKey :: Text
sortColKey = "sortDemoaCol"

sortValKey :: Text
sortValKey = "sortDemoaVal"

curDemoaListSortOpt :: Handler (Maybe (SortOpt Demoa))
curDemoaListSortOpt = do
  maybeSortCol <- lookupSession sortColKey
  maybeSortVal <- lookupSession sortValKey
  return $ case (maybeSortCol, maybeSortVal) of
    (Just sortCol, Just sortVal) -> case (sortCol, sortVal) of
      ("myattr", "asc") -> Just $ SortAsc DemoaMyattr
      ("myattr", "desc") -> Just $ SortDesc DemoaMyattr
      ("otherattr", "asc") -> Just $ SortAsc DemoaOtherattr
      ("otherattr", "desc") -> Just $ SortDesc DemoaOtherattr
      _ -> Nothing
    _ -> Nothing

postToggleSortDemoaMyattrR :: Int -> Handler Value
postToggleSortDemoaMyattrR pageNum = do
  maybeOldSortOpt <- curDemoaListSortOpt
  let newSortOpt = case maybeOldSortOpt of
        Just (SortAsc col@DemoaMyattr) -> Just $ SortDesc col
        Just (SortDesc DemoaMyattr) -> Nothing
        _ -> Just $ SortAsc DemoaMyattr
  case newSortOpt of
    Just sortOpt -> do
      setSession sortColKey "myattr"
      setSession sortValKey $ case sortOpt of
        SortAsc _ -> "asc"
        SortDesc _ -> "desc"
    _ -> do
      deleteSession sortColKey
      deleteSession sortValKey
  urlRenderer <- getUrlRender
  returnJson $
    VFormSubmitSuccess
      { fsSuccessDataJsonUrl = urlRenderer $ MyprojectR $ DemoaListPageNumDataR pageNum
      }

postToggleSortDemoaOtherattrR :: Int -> Handler Value
postToggleSortDemoaOtherattrR pageNum = do
  maybeOldSortOpt <- curDemoaListSortOpt
  let newSortOpt = case maybeOldSortOpt of
        Just (SortAsc col@DemoaOtherattr) -> Just $ SortDesc col
        Just (SortDesc DemoaOtherattr) -> Nothing
        _ -> Just $ SortAsc DemoaOtherattr
  case newSortOpt of
    Just sortOpt -> do
      setSession sortColKey "otherattr"
      setSession sortValKey $ case sortOpt of
        SortAsc _ -> "asc"
        SortDesc _ -> "desc"
    _ -> do
      deleteSession sortColKey
      deleteSession sortValKey
  urlRenderer <- getUrlRender
  returnJson $
    VFormSubmitSuccess
      { fsSuccessDataJsonUrl = urlRenderer $ MyprojectR $ DemoaListPageNumDataR pageNum
      }

postDemoaListPageNumDataR :: Int -> Handler Value
postDemoaListPageNumDataR pageNum = do
  urlRenderer <- getUrlRender
  returnJson $
    VFormSubmitSuccess
      { fsSuccessDataJsonUrl = urlRenderer $ MyprojectR $ DemoaListPageNumDataR pageNum
      }

getDemoaListPageNumDataR :: Int -> Handler Value
getDemoaListPageNumDataR = demoaListPageNumDataR

demoaListPageNumDataR :: Int -> Handler Value
demoaListPageNumDataR pageNum = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavDemoa
  (jDataDemoas, jDataPaginationItems) <- demoaListJDatas pageNum
  let pages =
        defaultDataPages
          { jDataPageDemoaList =
              Just $
                JDataPageDemoaList
                  { jDataPageDemoaListDemoas = jDataDemoas,
                    jDataPageDemoaListAddFormUrl = urlRenderer $ MyprojectR AddDemoaFormR,
                    jDataPageDemoaListPaginationItems = jDataPaginationItems,
                    jDataPageDemoaListMyattrToggleSortUrl = urlRenderer $ MyprojectR $ ToggleSortDemoaMyattrR pageNum,
                    jDataPageDemoaListOtherattrToggleSortUrl = urlRenderer $ MyprojectR $ ToggleSortDemoaOtherattrR pageNum
                  }
          }
  msgHome <- localizedMsg MsgGlobalHome
  msgDemoa <- localizedMsg MsgDemoaDemoa
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ MyprojectR DemoaListDataR
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
              { jDataHistoryStateUrl = urlRenderer $ MyprojectR DemoaListR,
                jDataHistoryStateTitle = msgDemoa
              },
        jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName,
        jDataCsrfParamName = defaultCsrfParamName,
        jDataCsrfToken = reqToken req,
        jDataBreadcrumbItems =
          [ JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgHome,
                jDataBreadcrumbItemDataUrl = urlRenderer $ MyprojectR HomeDataR
              },
            JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgDemoa,
                jDataBreadcrumbItemDataUrl = currentDataUrl
              }
          ],
        jDataCurrentLanguage = currentLanguage,
        jDataTranslation = translation,
        jDataLanguageDeUrl = urlRenderer $ MyprojectR $ LanguageDeR currentDataUrl,
        jDataLanguageEnUrl = urlRenderer $ MyprojectR $ LanguageEnR currentDataUrl
      }

demoaListJDatas :: Int -> Handler ([JDataDemoa], Maybe [JDataPaginationItem])
demoaListJDatas pageNum = do
  maybeSortOpt <- curDemoaListSortOpt
  urlRenderer <- getUrlRender
  rowCount <- runDB $ count ([] :: [Filter Demoa])
  paginationJDatas <- getPaginationJDatas rowCount demoaListPageSize pageNum 11 (MyprojectR . DemoaListPageNumDataR)
  demoaEnts <- runDB $ loadDemoaTuples maybeSortOpt
  let demoaJDatas =
        map
          ( \demoaEnt@(Entity demoaId _) ->
              JDataDemoa
                { jDataDemoaEnt = demoaEnt,
                  jDataDemoaEditFormUrl = urlRenderer $ MyprojectR $ EditDemoaFormR demoaId,
                  jDataDemoaDeleteFormUrl = urlRenderer $ MyprojectR $ DeleteDemoaFormR demoaId
                }
          )
          demoaEnts
  return (demoaJDatas, paginationJDatas)
  where
    sortOpt :: E.SqlExpr (Entity Demoa) -> Maybe (SortOpt Demoa) -> E.SqlExpr E.OrderBy
    sortOpt da maybeSortOpt = case maybeSortOpt of
      Just (SortAsc col) -> E.asc $ da E.^. col
      Just (SortDesc col) -> E.desc $ da E.^. col
      _ -> E.asc $ da E.^. DemoaId
    loadDemoaTuples :: Maybe (SortOpt Demoa) -> YesodDB App [(Entity Demoa)]
    loadDemoaTuples maybeSortOpt = do
      let pageSize = fromIntegral demoaListPageSize
      E.select $ E.from $ \(da) -> do
        E.orderBy [sortOpt da maybeSortOpt]
        E.offset ((fromIntegral pageNum - 1) * pageSize)
        E.limit pageSize
        return (da)

demoaListPageSize :: Int
demoaListPageSize = 5

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddDemoa = VAddDemoa
  { vAddDemoaMyattr :: Text,
    vAddDemoaOtherattr :: Maybe Text
  }

-- gen data add - end

-- gen get add form - start
getAddDemoaFormR :: Handler Html
getAddDemoaFormR = do
  (formWidget, _) <- generateFormPost $ vAddDemoaForm Nothing Nothing
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgDemoaAddDemoa}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{MyprojectR $ AddDemoaR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get add form - end

-- gen post add - start
postAddDemoaR :: Handler Value
postAddDemoaR = do
  ((result, formWidget), _) <- runFormPost $ vAddDemoaForm Nothing Nothing
  case result of
    FormSuccess vAddDemoa -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let demoa =
            Demoa
              { demoaMyattr = vAddDemoaMyattr vAddDemoa,
                demoaOtherattr = vAddDemoaOtherattr vAddDemoa,
                demoaVersion = 1,
                demoaCreatedAt = curTime,
                demoaCreatedBy = userIdent authUser,
                demoaUpdatedAt = curTime,
                demoaUpdatedBy = userIdent authUser
              }
      runDB $ do
        _ <- insert demoa
        return ()
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ MyprojectR DemoaListDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post add - end

-- gen add form - start
vAddDemoaForm :: Maybe DemoaId -> Maybe Demoa -> Html -> MForm Handler (FormResult VAddDemoa, Widget)
vAddDemoaForm maybeDemoaId maybeDemoa extra = do
  (myattrResult, myattrView) <-
    mreq
      textField
      myattrFs
      (demoaMyattr <$> maybeDemoa)
  (otherattrResult, otherattrView) <-
    mopt
      textField
      otherattrFs
      (demoaOtherattr <$> maybeDemoa)
  let vAddDemoaResult = VAddDemoa <$> myattrResult <*> otherattrResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div #myattrInputWidget .uk-margin-small :not $ null $ fvErrors myattrView:.uk-form-danger>
      <label #myattrInputLabel .uk-form-label :not $ null $ fvErrors myattrView:.uk-text-danger for=#{fvId myattrView}>#{fvLabel myattrView}
      <div .uk-form-controls>
        ^{fvInput myattrView}
        <span #myattrInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgDemoaMyattrInputInfo}
        $maybe err <- fvErrors myattrView
          <br>
          <span #myattrInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #otherattrInputWidget .uk-margin-small :not $ null $ fvErrors otherattrView:.uk-form-danger>
      <label #otherattrInputLabel .uk-form-label :not $ null $ fvErrors otherattrView:.uk-text-danger for=#{fvId otherattrView}>#{fvLabel otherattrView}
      <div .uk-form-controls>
        ^{fvInput otherattrView}
        <span #otherattrInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgDemoaOtherattrInputInfo}
        $maybe err <- fvErrors otherattrView
          <br>
          <span #otherattrInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vAddDemoaResult, formWidget)
  where
    myattrFs :: FieldSettings App
    myattrFs =
      FieldSettings
        { fsLabel = SomeMessage MsgDemoaMyattr,
          fsTooltip = Nothing,
          fsId = Just "myattr",
          fsName = Just "myattr",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    otherattrFs :: FieldSettings App
    otherattrFs =
      FieldSettings
        { fsLabel = SomeMessage MsgDemoaOtherattr,
          fsTooltip = Nothing,
          fsId = Just "otherattr",
          fsName = Just "otherattr",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }

-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditDemoa = VEditDemoa
  { vEditDemoaMyattr :: Text,
    vEditDemoaOtherattr :: Maybe Text,
    vEditDemoaVersion :: Int
  }

-- gen data edit - end

-- gen get edit form - start
getEditDemoaFormR :: DemoaId -> Handler Html
getEditDemoaFormR demoaId = do
  demoa <- runDB $ get404 demoaId
  (formWidget, _) <- generateFormPost $ vEditDemoaForm (Just demoaId) (Just demoa)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgDemoaEditDemoa}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{MyprojectR $ EditDemoaR demoaId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get edit form - end

-- gen post edit - start
postEditDemoaR :: DemoaId -> Handler Value
postEditDemoaR demoaId = do
  ((result, formWidget), _) <- runFormPost $ vEditDemoaForm (Just demoaId) Nothing
  case result of
    FormSuccess vEditDemoa -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields =
            [ DemoaMyattr =. vEditDemoaMyattr vEditDemoa,
              DemoaOtherattr =. vEditDemoaOtherattr vEditDemoa,
              DemoaVersion =. vEditDemoaVersion vEditDemoa + 1,
              DemoaUpdatedAt =. curTime,
              DemoaUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <-
          updateWhereCount
            [ DemoaId ==. demoaId,
              DemoaVersion ==. vEditDemoaVersion vEditDemoa
            ]
            persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ MyprojectR $ DemoaListDataR}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ MyprojectR $ DemoaListDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post edit - end

-- gen edit form - start
vEditDemoaForm :: Maybe DemoaId -> Maybe Demoa -> Html -> MForm Handler (FormResult VEditDemoa, Widget)
vEditDemoaForm maybeDemoaId maybeDemoa extra = do
  (myattrResult, myattrView) <-
    mreq
      textField
      myattrFs
      (demoaMyattr <$> maybeDemoa)
  (otherattrResult, otherattrView) <-
    mopt
      textField
      otherattrFs
      (demoaOtherattr <$> maybeDemoa)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (demoaVersion <$> maybeDemoa)
  let vEditDemoaResult = VEditDemoa <$> myattrResult <*> otherattrResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div #myattrInputWidget .uk-margin-small :not $ null $ fvErrors myattrView:.uk-form-danger>
      <label #myattrInputLabel .uk-form-label :not $ null $ fvErrors myattrView:.uk-text-danger for=#{fvId myattrView}>#{fvLabel myattrView}
      <div .uk-form-controls>
        ^{fvInput myattrView}
        <span #myattrInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgDemoaMyattrInputInfo}
        $maybe err <- fvErrors myattrView
          <br>
          <span #myattrInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #otherattrInputWidget .uk-margin-small :not $ null $ fvErrors otherattrView:.uk-form-danger>
      <label #otherattrInputLabel .uk-form-label :not $ null $ fvErrors otherattrView:.uk-text-danger for=#{fvId otherattrView}>#{fvLabel otherattrView}
      <div .uk-form-controls>
        ^{fvInput otherattrView}
        <span #otherattrInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgDemoaOtherattrInputInfo}
        $maybe err <- fvErrors otherattrView
          <br>
          <span #otherattrInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vEditDemoaResult, formWidget)
  where
    myattrFs :: FieldSettings App
    myattrFs =
      FieldSettings
        { fsLabel = SomeMessage MsgDemoaMyattr,
          fsTooltip = Nothing,
          fsId = Just "myattr",
          fsName = Just "myattr",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    otherattrFs :: FieldSettings App
    otherattrFs =
      FieldSettings
        { fsLabel = SomeMessage MsgDemoaOtherattr,
          fsTooltip = Nothing,
          fsId = Just "otherattr",
          fsName = Just "otherattr",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    versionFs :: FieldSettings App
    versionFs =
      FieldSettings
        { fsLabel = "",
          fsTooltip = Nothing,
          fsId = Just "version",
          fsName = Just "version",
          fsAttrs = []
        }

-- gen edit form - end

-------------------------------------------------------
-- delete
-------------------------------------------------------

-- gen get delete form - start
getDeleteDemoaFormR :: DemoaId -> Handler Html
getDeleteDemoaFormR demoaId = do
  (formWidget, _) <- generateFormPost $ vDeleteDemoaForm
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgDemoaDeleteDemoa}
      <form #modal-form .uk-form-horizontal method=post action=@{MyprojectR $ DeleteDemoaR demoaId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get delete form - end

-- gen post delete - start
postDeleteDemoaR :: DemoaId -> Handler Value
postDeleteDemoaR demoaId = do
  curTime <- liftIO getCurrentTime
  Entity _ authUser <- requireAuth
  runDB $ do
    -- trick to record the user deleting the entity
    updateWhere
      [DemoaId ==. demoaId]
      [ DemoaUpdatedAt =. curTime,
        DemoaUpdatedBy =. userIdent authUser
      ]
    delete demoaId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ MyprojectR $ DemoaListDataR}

-- gen post delete - end

-- gen delete form - start
vDeleteDemoaForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteDemoaForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
