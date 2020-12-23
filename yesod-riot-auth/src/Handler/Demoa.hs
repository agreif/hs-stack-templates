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
                    jDataPageDemoaListAddFormUrl = urlRenderer $ CrmR AddDemoaFormR,
                    jDataPageDemoaListPaginationItems = jDataPaginationItems
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
  urlRenderer <- getUrlRender
  rowCount <- runDB $ count ([] :: [Filter Demoa])
  paginationJDatas <- getPaginationJDatas rowCount demoaListPageSize pageNum 11 (MyprojectR . DemoaListPageNumDataR)
  demoaEnts <- runDB loadDemoaTuples
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
    loadDemoaTuples :: YesodDB App [(Entity Demoa)]
    loadDemoaTuples = do
      let pageSize = fromIntegral demoaListPageSize
      E.select $ E.from $ \(da) -> do
        E.orderBy [E.desc (da E.^. DemoaId)]
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
  { vAddDemoaMyattr :: Text
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
  let vAddDemoaResult = VAddDemoa <$> myattrResult
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

-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditDemoa = VEditDemoa
  { vEditDemoaMyattr :: Text,
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
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ MyprojectR DemoaListDataR}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ MyprojectR DemoaListDataR}
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
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (demoaVersion <$> maybeDemoa)
  let vEditDemoaResult = VEditDemoa <$> myattrResult <*> versionResult
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
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ MyprojectR DemoaListDataR}

-- gen post delete - end

-- gen delete form - start
vDeleteDemoaForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteDemoaForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
