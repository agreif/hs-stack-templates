{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Mailer where

import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy.Internal as LBS
import Handler.Common
import Import
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Shakespeare.Text (stext)

sendTestMail :: Text -> Handler ()
sendTestMail email = do
  appName <- runDB configAppName
  sendMail'
    email
    ("[" ++ appName ++ "] Test-Mail")
    (textPartContent)
    (htmlPartContent)
  where
    textPartContent = encodeUtf8 [stext| Test-Mail |]
    htmlPartContent = renderHtml [shamlet| Test-Mail |]

sendPasswordNewAccountMail :: User -> Text -> Handler ()
sendPasswordNewAccountMail user passwd = do
  loginUrl <- renderUrlToText $ AuthR LoginR
  appName <- runDB configAppName
  sendMail'
    (userEmail user)
    ("[" ++ appName ++ "] Neuer Nutzer angelegt")
    (textPartContent user appName loginUrl)
    (htmlPartContent user appName loginUrl)
  where
    textPartContent user' appName loginUrl =
      encodeUtf8
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
    htmlPartContent user' appName loginUrl =
      renderHtml
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
  appName <- runDB configAppName
  sendMail'
    (userEmail user)
    ("[" ++ appName ++ "] Ihr Passwort wurde zurückgesetzt")
    (textPartContent user passwd appName)
    (htmlPartContent user passwd appName)
  where
    textPartContent user' passwd' appName =
      encodeUtf8
        [stext|
Lieber #{appName} Nutzer,

Ihr Passwort wurde zurückgesetzt:

Anmeldename:    #{userIdent user'}
Neues Passwort: #{passwd'}

Mit freundlichen Grüßen,
Ihr #{appName} Team
      |]
    htmlPartContent user' passwd' appName =
      renderHtml
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
  appName <- runDB $ configAppName
  let mail =
        Mail
          { mailFrom = Address (Just appName) from,
            mailTo = [Address Nothing to],
            mailCc = [],
            mailBcc = [],
            mailHeaders =
              [ ("Subject", subject),
                ("Reply-To", from)
              ],
            mailParts = [[htmlPart', textPart']]
          }
  _ <- liftIO $ forkIO $ renderSendMailCustom "/run/wrappers/bin/sendmail" ["-t"] mail
  return ()
  where
    textPart' =
      Part
        { partType = "text/plain; charset=utf-8",
          partEncoding = None,
          partDisposition = DefaultDisposition,
          partHeaders = [],
          partContent = PartContent textPartContent
        }
    htmlPart' =
      Part
        { partType = "text/html; charset=utf-8",
          partEncoding = None,
          partDisposition = DefaultDisposition,
          partHeaders = [],
          partContent = PartContent htmlPartContent
        }
