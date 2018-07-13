{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Config where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)

-------------------------------------------------------
-- edit config
-------------------------------------------------------

-- gen data edit - start
data VEditConfig = VEditConfig
  { vEditConfigCode :: Maybe Text
  , vEditConfigStringValue :: Maybe Text
  , vEditConfigIntValue :: Maybe Int
  , vEditConfigDoubleValue :: Maybe Double
  , vEditConfigBoolValue :: Bool
  , vEditConfigVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditConfigFormR :: ConfigId -> Handler Html
getEditConfigFormR configId = do
  config <- runDB $ get404 configId
  (formWidget, _) <- generateFormPost $ vEditConfigForm $ Just config
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalEditConfig}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ EditConfigR configId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit form - start
postEditConfigR :: ConfigId -> Handler Value
postEditConfigR configId = do
  ((result, formWidget), _) <- runFormPost $ vEditConfigForm Nothing
  case result of
    FormSuccess vEditConfig -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            ConfigStringValue =. vEditConfigStringValue vEditConfig
            , ConfigIntValue =. vEditConfigIntValue vEditConfig
            , ConfigDoubleValue =. vEditConfigDoubleValue vEditConfig
            , ConfigBoolValue =. vEditConfigBoolValue vEditConfig
            , ConfigVersion =. vEditConfigVersion vEditConfig + 1
            , ConfigUpdatedAt =. curTime
            , ConfigUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ updateWhereCount [ ConfigId ==. configId
                                              , ConfigVersion ==. vEditConfigVersion vEditConfig
                                              ] persistFields
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit form - end

-- gen edit form - start
vEditConfigForm :: Maybe Config -> Html -> MForm Handler (FormResult VEditConfig, Widget)
vEditConfigForm maybeConfig extra = do
  (codeResult, codeView) <- mopt textField
    codeFs
    (Just $ configCode <$> maybeConfig)
  (stringValueResult, stringValueView) <- mopt textField
    stringValueFs
    (configStringValue <$> maybeConfig)
  (intValueResult, intValueView) <- mopt intField
    intValueFs
    (configIntValue <$> maybeConfig)
  (doubleValueResult, doubleValueView) <- mopt doubleField
    doubleValueFs
    (configDoubleValue <$> maybeConfig)
  (boolValueResult, boolValueView) <- mreq checkBoxField
    boolValueFs
    (configBoolValue <$> maybeConfig)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (configVersion <$> maybeConfig)
  let vEditConfigResult = VEditConfig <$> codeResult <*> stringValueResult <*> intValueResult <*> doubleValueResult <*> boolValueResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors codeView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors codeView:.uk-text-danger for=#{fvId codeView}>#{fvLabel codeView}
      <div .uk-form-controls>
        ^{fvInput codeView}
        $maybe err <- fvErrors codeView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors stringValueView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors stringValueView:.uk-text-danger for=#{fvId stringValueView}>#{fvLabel stringValueView}
      <div .uk-form-controls>
        ^{fvInput stringValueView}
        $maybe err <- fvErrors stringValueView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors intValueView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors intValueView:.uk-text-danger for=#{fvId intValueView}>#{fvLabel intValueView}
      <div .uk-form-controls>
        ^{fvInput intValueView}
        $maybe err <- fvErrors intValueView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors doubleValueView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors doubleValueView:.uk-text-danger for=#{fvId doubleValueView}>#{fvLabel doubleValueView}
      <div .uk-form-controls>
        ^{fvInput doubleValueView}
        $maybe err <- fvErrors doubleValueView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors boolValueView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors boolValueView:.uk-text-danger for=#{fvId boolValueView}>#{fvLabel boolValueView}
      <div .uk-form-controls>
        ^{fvInput boolValueView}
        $maybe err <- fvErrors boolValueView
          &nbsp;#{err}
    |]
  return (vEditConfigResult, formWidget)
  where
    codeFs :: FieldSettings App
    codeFs = FieldSettings
      { fsLabel = SomeMessage MsgEditConfigCode
      , fsTooltip = Nothing
      , fsId = Just "code"
      , fsName = Just "code"
      , fsAttrs = [ ("disabled",""), ("class","uk-form-width-large uk-input uk-form-small") ]
      }
    stringValueFs :: FieldSettings App
    stringValueFs = FieldSettings
      { fsLabel = SomeMessage MsgEditConfigStringValue
      , fsTooltip = Nothing
      , fsId = Just "stringValue"
      , fsName = Just "stringValue"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
    intValueFs :: FieldSettings App
    intValueFs = FieldSettings
      { fsLabel = SomeMessage MsgEditConfigIntValue
      , fsTooltip = Nothing
      , fsId = Just "intValue"
      , fsName = Just "intValue"
      , fsAttrs = [ ("class","uk-form-width-medium uk-input uk-form-small") ]
      }
    doubleValueFs :: FieldSettings App
    doubleValueFs = FieldSettings
      { fsLabel = SomeMessage MsgEditConfigDoubleValue
      , fsTooltip = Nothing
      , fsId = Just "doubleValue"
      , fsName = Just "doubleValue"
      , fsAttrs = [ ("class","uk-form-width-medium uk-input uk-form-small") ]
      }
    boolValueFs :: FieldSettings App
    boolValueFs = FieldSettings
      { fsLabel = SomeMessage MsgEditConfigBoolValue
      , fsTooltip = Nothing
      , fsId = Just "boolValue"
      , fsName = Just "boolValue"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }

data MsgEditConfig =
  MsgEditConfigCode
  | MsgEditConfigStringValue
  | MsgEditConfigIntValue
  | MsgEditConfigDoubleValue
  | MsgEditConfigBoolValue

instance RenderMessage App MsgEditConfig where
  renderMessage _ []        = renderEditConfigGerman
  renderMessage _ ("de":_) = renderEditConfigGerman
  renderMessage _ ("en":_) = renderEditConfigEnglish
  renderMessage _ ("en-US":_) = renderEditConfigEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderEditConfigGerman :: MsgEditConfig -> Text
renderEditConfigGerman MsgEditConfigCode = "Code"
renderEditConfigGerman MsgEditConfigStringValue = "String-Wert"
renderEditConfigGerman MsgEditConfigIntValue = "Integer-Wert"
renderEditConfigGerman MsgEditConfigDoubleValue = "Double-Wert"
renderEditConfigGerman MsgEditConfigBoolValue = "Boolean-Wert"


renderEditConfigEnglish :: MsgEditConfig -> Text
renderEditConfigEnglish MsgEditConfigCode = "Code"
renderEditConfigEnglish MsgEditConfigStringValue = "String-Value"
renderEditConfigEnglish MsgEditConfigIntValue = "Integer-Value"
renderEditConfigEnglish MsgEditConfigDoubleValue = "Double-Value"
renderEditConfigEnglish MsgEditConfigBoolValue = "Boolean-Value"

-- gen edit form - end
