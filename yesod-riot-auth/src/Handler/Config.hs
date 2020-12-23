{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Config where

import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-------------------------------------------------------
-- edit config
-------------------------------------------------------

-- gen data edit - start
data VEditConfig = VEditConfig
  { vEditConfigCode :: Maybe Text,
    vEditConfigStringValue :: Maybe Text,
    vEditConfigIntValue :: Maybe Int,
    vEditConfigDoubleValue :: Maybe Double,
    vEditConfigBoolValue :: Bool,
    vEditConfigVersion :: Int
  }

-- gen data edit - end

-- gen get edit form - start
getEditConfigFormR :: ConfigId -> Handler Html
getEditConfigFormR configId = do
  config <- runDB $ get404 configId
  (formWidget, _) <- generateFormPost $ vEditConfigForm (Just configId) (Just config)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgConfigEditConfig}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ EditConfigR configId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get edit form - end

-- gen post edit form - start
postEditConfigR :: ConfigId -> Handler Value
postEditConfigR configId = do
  ((result, formWidget), _) <- runFormPost $ vEditConfigForm (Just configId) Nothing
  case result of
    FormSuccess vEditConfig -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields =
            [ ConfigStringValue =. vEditConfigStringValue vEditConfig,
              ConfigIntValue =. vEditConfigIntValue vEditConfig,
              ConfigDoubleValue =. vEditConfigDoubleValue vEditConfig,
              ConfigBoolValue =. vEditConfigBoolValue vEditConfig,
              ConfigVersion =. vEditConfigVersion vEditConfig + 1,
              ConfigUpdatedAt =. curTime,
              ConfigUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <-
          updateWhereCount
            [ ConfigId ==. configId,
              ConfigVersion ==. vEditConfigVersion vEditConfig
            ]
            persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ AdminR AdminDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post edit form - end

-- gen edit form - start
vEditConfigForm :: Maybe ConfigId -> Maybe Config -> Html -> MForm Handler (FormResult VEditConfig, Widget)
vEditConfigForm maybeConfigId maybeConfig extra = do
  (codeResult, codeView) <-
    mopt
      textField
      codeFs
      (Just $ configCode <$> maybeConfig)
  (stringValueResult, stringValueView) <-
    mopt
      textField
      stringValueFs
      (configStringValue <$> maybeConfig)
  (intValueResult, intValueView) <-
    mopt
      intField
      intValueFs
      (configIntValue <$> maybeConfig)
  (doubleValueResult, doubleValueView) <-
    mopt
      doubleField
      doubleValueFs
      (configDoubleValue <$> maybeConfig)
  (boolValueResult, boolValueView) <-
    mreq
      checkBoxField
      boolValueFs
      (configBoolValue <$> maybeConfig)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (configVersion <$> maybeConfig)
  let vEditConfigResult = VEditConfig <$> codeResult <*> stringValueResult <*> intValueResult <*> doubleValueResult <*> boolValueResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div #codeInputWidget .uk-margin-small :not $ null $ fvErrors codeView:.uk-form-danger>
      <label #codeInputLabel .uk-form-label :not $ null $ fvErrors codeView:.uk-text-danger for=#{fvId codeView}>#{fvLabel codeView}
      <div .uk-form-controls>
        ^{fvInput codeView}
        <span #codeInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgConfigCodeInputInfo}
        $maybe err <- fvErrors codeView
          <br>
          <span #codeInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #stringValueInputWidget .uk-margin-small :not $ null $ fvErrors stringValueView:.uk-form-danger>
      <label #stringValueInputLabel .uk-form-label :not $ null $ fvErrors stringValueView:.uk-text-danger for=#{fvId stringValueView}>#{fvLabel stringValueView}
      <div .uk-form-controls>
        ^{fvInput stringValueView}
        <span #stringValueInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgConfigStringValueInputInfo}
        $maybe err <- fvErrors stringValueView
          <br>
          <span #stringValueInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #intValueInputWidget .uk-margin-small :not $ null $ fvErrors intValueView:.uk-form-danger>
      <label #intValueInputLabel .uk-form-label :not $ null $ fvErrors intValueView:.uk-text-danger for=#{fvId intValueView}>#{fvLabel intValueView}
      <div .uk-form-controls>
        ^{fvInput intValueView}
        <span #intValueInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgConfigIntValueInputInfo}
        $maybe err <- fvErrors intValueView
          <br>
          <span #intValueInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #doubleValueInputWidget .uk-margin-small :not $ null $ fvErrors doubleValueView:.uk-form-danger>
      <label #doubleValueInputLabel .uk-form-label :not $ null $ fvErrors doubleValueView:.uk-text-danger for=#{fvId doubleValueView}>#{fvLabel doubleValueView}
      <div .uk-form-controls>
        ^{fvInput doubleValueView}
        <span #doubleValueInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgConfigDoubleValueInputInfo}
        $maybe err <- fvErrors doubleValueView
          <br>
          <span #doubleValueInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #boolValueInputWidget .uk-margin-small :not $ null $ fvErrors boolValueView:.uk-form-danger>
      <label #boolValueInputLabel .uk-form-label :not $ null $ fvErrors boolValueView:.uk-text-danger for=#{fvId boolValueView}>#{fvLabel boolValueView}
      <div .uk-form-controls>
        ^{fvInput boolValueView}
        <span #boolValueInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgConfigBoolValueInputInfo}
        $maybe err <- fvErrors boolValueView
          <br>
          <span #boolValueInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vEditConfigResult, formWidget)
  where
    codeFs :: FieldSettings App
    codeFs =
      FieldSettings
        { fsLabel = SomeMessage MsgConfigCode,
          fsTooltip = Nothing,
          fsId = Just "code",
          fsName = Just "code",
          fsAttrs = [("disabled", ""), ("class", "uk-input uk-form-small uk-form-width-large")]
        }
    stringValueFs :: FieldSettings App
    stringValueFs =
      FieldSettings
        { fsLabel = SomeMessage MsgConfigStringValue,
          fsTooltip = Nothing,
          fsId = Just "stringValue",
          fsName = Just "stringValue",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    intValueFs :: FieldSettings App
    intValueFs =
      FieldSettings
        { fsLabel = SomeMessage MsgConfigIntValue,
          fsTooltip = Nothing,
          fsId = Just "intValue",
          fsName = Just "intValue",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    doubleValueFs :: FieldSettings App
    doubleValueFs =
      FieldSettings
        { fsLabel = SomeMessage MsgConfigDoubleValue,
          fsTooltip = Nothing,
          fsId = Just "doubleValue",
          fsName = Just "doubleValue",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    boolValueFs :: FieldSettings App
    boolValueFs =
      FieldSettings
        { fsLabel = SomeMessage MsgConfigBoolValue,
          fsTooltip = Nothing,
          fsId = Just "boolValue",
          fsName = Just "boolValue",
          fsAttrs = [("class", "uk-checkbox")]
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
