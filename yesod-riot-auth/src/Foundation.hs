{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)
import Yesod.Auth.Message   (AuthMessage(InvalidLogin))
import Control.Monad.Trans.Maybe
import Yesod.Auth.HashDB
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Form.I18n.German
import qualified Data.Maybe as M (fromJust)
import I18n

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m, Functor m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_local_css
            addScript $ StaticR js_local_js
            widget
        withUrlRenderer $(hamletFile "templates/riot-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized (HomeR) _ = do
        _ <- requireAuthId
        return Authorized
    isAuthorized (MyprojectR _) _ = do
        _ <- requireAuthId
        return Authorized
    isAuthorized (AdminR _) _ = do
        userId <- requireAuthId
        user <- runDB $ get404 userId
        return $ case userIsAdmin user of
                   True -> Authorized
                   False -> Unauthorized "Admins Only!"
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

formLayout :: Widget -> Handler Html
formLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet|^{pageBody pc}|]

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    authLayout widget = do
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_local_css
            addScript $ StaticR js_local_js
            widget
        withUrlRenderer $(hamletFile "templates/login-layout-wrapper.hamlet")

    loginHandler = do
      maybeAppName <- lift $ runDB $ maybeConfigText "app_name"
      tp <- getRouteToParent
      lift $ authLayout $ do
        when (isJust maybeAppName) $ setTitle $ toHtml $ M.fromJust maybeAppName
        master <- getYesod
        mapM_ (flip apLogin tp) (authPlugins master)

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = False
    -- When being redirected to the login page should the current page be set to redirect back to
    redirectToCurrent :: App -> Bool
    redirectToCurrent _ = False

    -- authenticate :: (MonadHandler m, HandlerSite m ~ App)
    --              => Creds App -> m (AuthenticationResult App)
    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> return $ UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authHashDBWithForm myLoginForm (Just . UniqueUser)]

    -- authPlugins app = [authOpenId Claimed []] ++ extraAuthPlugins
    --     -- Enable authDummy login if enabled.
    --     where extraAuthPlugins = []

    authHttpManager = getHttpManager

    -- override, to avoid DB lookup on every request
    maybeAuthId = runMaybeT $ do
      s   <- MaybeT $ lookupSession credsKey
      aid <- MaybeT $ return $ fromPathPiece s
      return aid

myLoginForm :: Route App -> Widget
myLoginForm loginRoute = do
  request <- getRequest
  let maybeToken = reqToken request
  $(whamletFile "templates/login_form.hamlet")

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = Just h }

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
-- instance RenderMessage App FormMessage where
--     renderMessage :: App -> [Lang] -> FormMessage -> Text
--     renderMessage _ _ = defaultFormMessage

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ []        = germanFormMessage -- Default to German
    renderMessage _ ("de":_) = germanFormMessage
    renderMessage _ ("en-US":_) = defaultFormMessage
    renderMessage _ ("en":_) = defaultFormMessage
    renderMessage master (_   :langs) = renderMessage master langs

instance RenderMessage App AppMessage where
    renderMessage _ []        = renderMessageGerman
    renderMessage _ ("de":_) = renderMessageGerman
    renderMessage _ ("en":_) = renderMessageEnglish
    renderMessage _ ("en-US":_) = renderMessageEnglish
    renderMessage m (_:ls) = renderMessage m ls

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

--------------------------------------------------------------------------------
-- config helpers
--------------------------------------------------------------------------------

maybeConfigText :: Text -> YesodDB App (Maybe Text)
maybeConfigText code = do
  maybeConfigEnt <- selectFirst [ConfigCode ==. code] []
  case maybeConfigEnt of
    Just (Entity _ (Config {configStringValue = result})) -> return result
    Nothing -> return Nothing

maybeConfigInt :: Text -> YesodDB App (Maybe Int)
maybeConfigInt code = do
  maybeConfigEnt <- selectFirst [ConfigCode ==. code] []
  case maybeConfigEnt of
    Just (Entity _ (Config {configIntValue = result})) -> return result
    Nothing -> return Nothing

maybeConfigDouble :: Text -> YesodDB App (Maybe Double)
maybeConfigDouble code = do
  maybeConfigEnt <- selectFirst [ConfigCode ==. code] []
  case maybeConfigEnt of
    Just (Entity _ (Config {configDoubleValue = result})) -> return result
    Nothing -> return Nothing

configBool :: Text -> YesodDB App Bool
configBool code = do
  maybeConfigEnt <- selectFirst [ConfigCode ==. code] []
  case maybeConfigEnt of
    Just (Entity _ (Config {configBoolValue = result})) -> return result
    Nothing -> return False

configAppName :: YesodDB App Text
configAppName = do
  maybeResult <- maybeConfigText "app_name"
  case maybeResult  of
    Just result -> return result
    Nothing -> return ""

configEmailFrom :: YesodDB App Text
configEmailFrom = do
  maybeResult <- maybeConfigText "email_from"
  case maybeResult  of
    Just result -> return result
    Nothing -> return ""

configMehrwertSteuer :: YesodDB App Double
configMehrwertSteuer = do
  maybeResult <- maybeConfigDouble "mehrwert_steuer"
  case maybeResult  of
    Just result -> return result
    Nothing -> return 0
