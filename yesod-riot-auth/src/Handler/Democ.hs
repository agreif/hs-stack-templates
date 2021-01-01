{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Democ where

import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddDemoc = VAddDemoc
  { vAddDemocMyattr :: Text
  }

-- gen data add - end

-- gen get add form - start
getAddDemocFormR :: DemobId -> Handler Html
getAddDemocFormR demobId = do
  (formWidget, _) <- generateFormPost $ vAddDemocForm Nothing Nothing
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgDemocAddDemoc}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{BackendR $ AddDemocR demobId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get add form - end

-- gen post add - start
postAddDemocR :: DemobId -> Handler Value
postAddDemocR demobId = do
  ((result, formWidget), _) <- runFormPost $ vAddDemocForm Nothing Nothing
  case result of
    FormSuccess vAddDemoc -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let democ =
            Democ
              { democDemobId = demobId,
                democMyattr = vAddDemocMyattr vAddDemoc,
                democVersion = 1,
                democCreatedAt = curTime,
                democCreatedBy = userIdent authUser,
                democUpdatedAt = curTime,
                democUpdatedBy = userIdent authUser
              }
      runDB $ do
        _ <- insert democ
        return ()
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ BackendR $ DemobDetailDataR demobId}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post add - end

-- gen add form - start
vAddDemocForm :: Maybe DemocId -> Maybe Democ -> Html -> MForm Handler (FormResult VAddDemoc, Widget)
vAddDemocForm maybeDemocId maybeDemoc extra = do
  (myattrResult, myattrView) <-
    mreq
      textField
      myattrFs
      (democMyattr <$> maybeDemoc)
  let vAddDemocResult = VAddDemoc <$> myattrResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div #myattrInputWidget .uk-margin-small :not $ null $ fvErrors myattrView:.uk-form-danger>
      <label #myattrInputLabel .uk-form-label :not $ null $ fvErrors myattrView:.uk-text-danger for=#{fvId myattrView}>#{fvLabel myattrView}
      <div .uk-form-controls>
        ^{fvInput myattrView}
        <span #myattrInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgDemocMyattrInputInfo}
        $maybe err <- fvErrors myattrView
          <br>
          <span #myattrInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vAddDemocResult, formWidget)
  where
    myattrFs :: FieldSettings App
    myattrFs =
      FieldSettings
        { fsLabel = SomeMessage MsgDemocMyattr,
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
data VEditDemoc = VEditDemoc
  { vEditDemocMyattr :: Text,
    vEditDemocVersion :: Int
  }

-- gen data edit - end

-- gen get edit form - start
getEditDemocFormR :: DemocId -> Handler Html
getEditDemocFormR democId = do
  democ <- runDB $ get404 democId
  (formWidget, _) <- generateFormPost $ vEditDemocForm (Just democId) (Just democ)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgDemocEditDemoc}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{BackendR $ EditDemocR democId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get edit form - end

-- gen post edit - start
postEditDemocR :: DemocId -> Handler Value
postEditDemocR democId = do
  ((result, formWidget), _) <- runFormPost $ vEditDemocForm (Just democId) Nothing
  case result of
    FormSuccess vEditDemoc -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      democ <- runDB $ get404 democId
      let persistFields =
            [ DemocMyattr =. vEditDemocMyattr vEditDemoc,
              DemocVersion =. vEditDemocVersion vEditDemoc + 1,
              DemocUpdatedAt =. curTime,
              DemocUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <-
          updateWhereCount
            [ DemocId ==. democId,
              DemocVersion ==. vEditDemocVersion vEditDemoc
            ]
            persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ BackendR $ DemobDetailDataR $ democDemobId democ}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ BackendR $ DemobDetailDataR $ democDemobId democ}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post edit - end

-- gen edit form - start
vEditDemocForm :: Maybe DemocId -> Maybe Democ -> Html -> MForm Handler (FormResult VEditDemoc, Widget)
vEditDemocForm maybeDemocId maybeDemoc extra = do
  (myattrResult, myattrView) <-
    mreq
      textField
      myattrFs
      (democMyattr <$> maybeDemoc)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (democVersion <$> maybeDemoc)
  let vEditDemocResult = VEditDemoc <$> myattrResult <*> versionResult
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
          _{MsgDemocMyattrInputInfo}
        $maybe err <- fvErrors myattrView
          <br>
          <span #myattrInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vEditDemocResult, formWidget)
  where
    myattrFs :: FieldSettings App
    myattrFs =
      FieldSettings
        { fsLabel = SomeMessage MsgDemocMyattr,
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
getDeleteDemocFormR :: DemocId -> Handler Html
getDeleteDemocFormR democId = do
  (formWidget, _) <- generateFormPost $ vDeleteDemocForm
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgDemocDeleteDemoc}
      <form #modal-form .uk-form-horizontal method=post action=@{BackendR $ DeleteDemocR democId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get delete form - end

-- gen post delete - start
postDeleteDemocR :: DemocId -> Handler Value
postDeleteDemocR democId = do
  curTime <- liftIO getCurrentTime
  Entity _ authUser <- requireAuth
  democ <- runDB $ get404 democId
  runDB $ do
    -- trick to record the user deleting the entity
    updateWhere
      [DemocId ==. democId]
      [ DemocUpdatedAt =. curTime,
        DemocUpdatedBy =. userIdent authUser
      ]
    delete democId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ BackendR $ DemobDetailDataR $ democDemobId democ}

-- gen post delete - end

-- gen delete form - start
vDeleteDemocForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteDemocForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
