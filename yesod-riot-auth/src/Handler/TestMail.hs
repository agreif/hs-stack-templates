{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.TestMail where

import Handler.Common
import Handler.Mailer
import Import

import qualified Text.Blaze.Html.Renderer.Text as Blaze

data Testmail = Testmail
  { testmailEmail :: Text
  }

postAddTestmailR :: Handler Value
postAddTestmailR = do
  ((result, formWidget), _) <- runFormPost $ vAddTestmailForm Nothing
  case result of
    FormSuccess vAddTestmail -> do
      urlRenderer <- getUrlRender
      sendTestMail $ vAddTestmailEmail vAddTestmail
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen data add - start
data VAddTestmail = VAddTestmail
  { vAddTestmailEmail :: Text
  }
-- gen data add - end

-- gen get add form - start
getAddTestmailFormR :: Handler Html
getAddTestmailFormR = do
  (formWidget, _) <- generateFormPost $ vAddTestmailForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgTestmailSendTestMail}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ AddTestmailR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen add form - start
vAddTestmailForm :: Maybe Testmail -> Html -> MForm Handler (FormResult VAddTestmail, Widget)
vAddTestmailForm maybeTestmail extra = do
  (emailResult, emailView) <- mreq textField
    emailFs
    (testmailEmail <$> maybeTestmail)
  let vAddTestmailResult = VAddTestmail <$> emailResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors emailView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors emailView:.uk-text-danger for=#{fvId emailView}>#{fvLabel emailView}
      <div .uk-form-controls>
        ^{fvInput emailView}
        $maybe err <- fvErrors emailView
          &nbsp;#{err}
    |]
  return (vAddTestmailResult, formWidget)
  where
    emailFs :: FieldSettings App
    emailFs = FieldSettings
      { fsLabel = SomeMessage MsgTestmailEmail
      , fsTooltip = Nothing
      , fsId = Just "email"
      , fsName = Just "email"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
-- gen add form - end
