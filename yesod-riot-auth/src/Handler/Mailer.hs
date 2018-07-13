{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Mailer where

import Import
import Handler.Common
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy.Internal as LBS
import Control.Concurrent (forkIO)

sendTestMail :: Text -> Handler ()
sendTestMail email = do
  do
    appName <- runDB $ configAppName
    sendMail' email
      ("[" ++ appName ++ "] Test-Mail")
      (textPartContent)
      (htmlPartContent)
  where
    textPartContent = encodeUtf8 [stext| Test-Mail |]
    htmlPartContent = renderHtml [shamlet| Test-Mail |]

sendPasswordNewAccountMail :: User -> Text -> Handler ()
sendPasswordNewAccountMail user passwd = do
  do
    loginUrl <- renderUrlToText $ AuthR LoginR
    appName <- runDB $ configAppName
    sendMail' (userEmail user)
      ("[" ++ appName ++ "] Neuer Nutzer angelegt")
      (textPartContent user appName loginUrl)
      (htmlPartContent user appName loginUrl)
  where
    textPartContent user' appName loginUrl = encodeUtf8
      [stext|
Lieber #{appName} Nutzer,

Für Sie wurde ein neuer Nutzer mit folgenden Daten angelegt:

Anmeldename:    #{userIdent user'}
Email:          #{userEmail user'}
Passwort:       #{passwd}

Sie können sich ab jetzt auf der #{appName} Seite einloggen:
#{loginUrl}

Mit freundlichen Grüßen,
Ihr #{appName} Team
      |]
    htmlPartContent user' appName loginUrl = renderHtml
      [shamlet|
<p>Lieber #{appName} Nutzer,
<p>Für Sie wurde ein neuer Nutzer mit folgenden Daten angelegt:
<p>Anmeldename:    #{userIdent user'}
  <br>
  Email:          #{userEmail user'}
  <br>
  Passwort:       #{passwd}
<p>Sie können sich ab jetzt auf der #{appName} Seite einloggen:
  <br>
  #{loginUrl}
<p>Mit freundlichen Grüßen,
  <br>
  Ihr #{appName} Team
      |]

sendPasswordResetMail :: User -> Text -> Handler ()
sendPasswordResetMail user passwd = do
  appName <- runDB $ configAppName
  sendMail' (userEmail user)
    ("[" ++ appName ++ "] Ihr Passwort wurde zurückgesetzt")
    (textPartContent user passwd appName)
    (htmlPartContent user passwd appName)
  where
    textPartContent user' passwd' appName = encodeUtf8
      [stext|
Lieber #{appName} Nutzer,

Ihr Passwort wurde zurückgesetzt:

Anmeldename:    #{userIdent user'}
Neues Passwort: #{passwd'}

Mit freundlichen Grüßen,
Ihr #{appName} Team
      |]
    htmlPartContent user' passwd' appName = renderHtml
      [shamlet|
<p>Lieber #{appName} Nutzer,
<p>Ihr Passwort wurde zurückgesetzt:
<p>Anmeldename:    #{userIdent user'}
  <br>
  Neues Passwort: #{passwd'}
<p>Mit freundlichen Grüßen,
  <br>
  Ihr #{appName} Team
      |]

sendMail' :: Text -> Text -> LBS.ByteString -> LBS.ByteString -> Handler ()
sendMail' to subject textPartContent htmlPartContent = do
  from <- runDB $ configEmailFrom
  let mail = Mail
        { mailFrom = Address Nothing from
        , mailTo = [Address Nothing to]
        , mailCc = []
        , mailBcc = []
        , mailHeaders =
            [ ("Subject", subject),
              ("Reply-To", from)
            ]
        , mailParts = [[textPart', htmlPart']]
        }

  _ <- liftIO $ forkIO $ renderSendMailCustom "/run/wrappers/bin/sendmail" ["-t"] mail
  return ()

  -- _ <- liftIO $ forkIO $ renderSendMail (emptyMail $ Address Nothing from)
  --   { mailTo = [Address Nothing to]
  --   , mailHeaders =
  --        [ ("Subject", subject),
  --          ("Reply-To", from)
  --        ]
  --   , mailParts = [[textPart', htmlPart']] }
  -- return ()
  where
    textPart' = Part { partType = "text/plain; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partContent = textPartContent
                     , partHeaders = [] }
    htmlPart' = Part { partType = "text/html; charset=utf-8"
                     , partEncoding = None
                     , partFilename = Nothing
                     , partContent = htmlPartContent
                     , partHeaders = [] }
