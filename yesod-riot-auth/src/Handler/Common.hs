{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import qualified Crypto.PasswordStore as Crypto
import qualified Data.ByteString.Char8 as BSC
import Control.Monad.Random
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import Text.Printf
import qualified Data.Maybe as M
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as B
import Data.Time

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


data VPostSubmitSuccess = VPostSubmitSuccess
  { fsPostSuccessDataJsonUrl :: Text
  }
instance ToJSON VPostSubmitSuccess where
  toJSON o = object
    [ "dataJsonUrl" .= fsPostSuccessDataJsonUrl o
    ]

data VFormSubmitSuccess = VFormSubmitSuccess
  { fsSuccessDataJsonUrl :: Text
  }
instance ToJSON VFormSubmitSuccess where
  toJSON o = object
    [ "isFormValid" .= True
    , "dataJsonUrl" .= fsSuccessDataJsonUrl o
    ]

data VFormSubmitInvalid = VFormSubmitInvalid
  { fsInvalidModalWidgetHtml :: Text
  }
instance ToJSON VFormSubmitInvalid where
  toJSON o = object
    [ "modalWidgetHtml" .= fsInvalidModalWidgetHtml o
    ]

data VFormSubmitStale = VFormSubmitStale
  { fsStaleDataJsonUrl :: Text
  }
instance ToJSON VFormSubmitStale where
  toJSON o = object
    [ "isStaleObjectState" .= True
    , "dataJsonUrl" .= fsStaleDataJsonUrl o
    ]


data JData = JData
  { jDataAppName :: Text
  , jDataUserIdent :: Text
  , jDataMainNavItems :: [JDataNavItem]
  , jDataSubNavItems :: [JDataNavItem]
  , jDataPages :: JDataPages
  , jDataHistoryState :: Maybe JDataHistoryState
  , jDataCsrfToken :: Maybe Text
  , jDataCsrfHeaderName :: Text
  , jDataBreadcrumbItems :: [JDataBreadcrumbItem]
  }
instance ToJSON JData where
  toJSON o = object
    [ "appName" .= jDataAppName o
    , "userIdent" .= jDataUserIdent o
    , "mainNavItems" .= jDataMainNavItems o
    , "subNavItems" .= jDataSubNavItems o
    , "pages" .= jDataPages o
    , "historyState" .= jDataHistoryState o
    , "csrfHeaderName" .= jDataCsrfHeaderName o
    , "csrfToken" .= jDataCsrfToken o
    , "breadcrumbItems" .= jDataBreadcrumbItems o
    ]

data JDataNavItem = JDataNavItem
  { jDataNavItemLabel :: Text
  , jDataNavItemIsActive :: Bool
  , jDataNavItemPageDataUrl :: Text
  , jDataNavItemBadge :: Maybe Text
  }
instance ToJSON JDataNavItem where
  toJSON o = object
    [ "label" .= jDataNavItemLabel o
    , "isActive" .= jDataNavItemIsActive o
    , "dataUrl" .= jDataNavItemPageDataUrl o
    , "badge" .= jDataNavItemBadge o
    ]


data JDataBreadcrumbItem = JDataBreadcrumbItem
  { jDataBreadcrumbItemLabel :: Text
  , jDataBreadcrumbItemDataUrl :: Text
  }
instance ToJSON JDataBreadcrumbItem where
  toJSON o = object
    [ "label" .= jDataBreadcrumbItemLabel o
    , "dataUrl" .= jDataBreadcrumbItemDataUrl o
    ]


data JDataHistoryState = JDataHistoryState
  { jDataHistoryStateUrl :: Text
  , jDataHistoryStateTitle :: Text
  }
instance ToJSON JDataHistoryState where
  toJSON o = object
    [ "url" .= jDataHistoryStateUrl o
    , "title" .= jDataHistoryStateTitle o
    ]

instance ToJSON User where
  toJSON o = object
    [ "ident" .= userIdent o
    , "email" .= userEmail o
    ]

data JDataPages = JDataPages
  { jDataPageHome :: Maybe JDataPageHome
  , jDataPageAdmin :: Maybe JDataPageAdmin
  }
instance ToJSON JDataPages where
  toJSON o = object
    [ "home" .= jDataPageHome o
    , "admin" .= jDataPageAdmin o
    ]

defaultDataPages :: JDataPages
defaultDataPages = JDataPages
  { jDataPageHome = Nothing
  , jDataPageAdmin = Nothing
  }


data JDataPageHome = JDataPageHome
  { jDataPageHomeContent :: Text
  }
instance ToJSON JDataPageHome where
  toJSON o = object
    [ "content" .= jDataPageHomeContent o
    ]


data JDataPageAdmin = JDataPageAdmin
  { jDataPageAdminUsers :: [JDataUser]
  , jDataPageAdminConfigs :: [JDataConfig]
  }
instance ToJSON JDataPageAdmin where
  toJSON o = object
    [ "users" .= jDataPageAdminUsers o
    , "configs" .= jDataPageAdminConfigs o
    ]


data JDataUser = JDataUser
  { jDataUserEnt :: Entity User
  , jDataUserEditFormUrl :: Text
  , jDataUserDeleteFormUrl :: Text
  }
instance ToJSON JDataUser where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataUserEnt o)
    , "editFormUrl" .= jDataUserEditFormUrl o
    , "deleteFormUrl" .= jDataUserDeleteFormUrl o
    ]


data JDataConfig = JDataConfig
  { jDataConfigEnt :: Entity Config
  , jDataConfigEditFormUrl :: Text
  }
instance ToJSON JDataConfig where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataConfigEnt o)
    , "editFormUrl" .= jDataConfigEditFormUrl o
    ]






--------------------------------------------------------------------------------
-- navigation helpers
--------------------------------------------------------------------------------

data MainNav
  = MainNavHome
  | MainNavAdmin
  deriving (Eq)

mainNavData :: MainNav -> Handler [JDataNavItem]
mainNavData mainNav = do
  urlRenderer <- getUrlRender
  msgHome <- localizedMsg MsgGlobalHome
  msgAdmin <- localizedMsg MsgGlobalAdmin
  return
    [ JDataNavItem
      { jDataNavItemLabel = msgHome
      , jDataNavItemIsActive = mainNav == MainNavHome
      , jDataNavItemPageDataUrl = urlRenderer $ SimulationR HomePageDataJsonR
      , jDataNavItemBadge = Nothing
      }
    , JDataNavItem
      { jDataNavItemLabel = msgAdmin
      , jDataNavItemIsActive = mainNav == MainNavAdmin
      , jDataNavItemPageDataUrl = urlRenderer $ SimulationR AdminPageDataJsonR
      , jDataNavItemBadge = Nothing
      }
    ]
  where

rnd :: (RandomGen g) => Int -> Int -> Rand g Int
rnd x y = getRandomR (x, y)

randomMixedCaseString :: Int -> IO String
randomMixedCaseString len = do
  values <- evalRandIO (sequence (replicate len $ rnd 65 90))
  let str = toLower $ map C.chr values
  -- in average in every 2 chars is an uppercase char
  str' <- upcaseChars (quot len 2) str
  return str'
  where
    upcaseChars :: Int -> String -> IO String
    upcaseChars countChars str =
      if countChars == 0 then
        return str
      else do
        p <- evalRandIO $ rnd 0 (length str - 2)
        let (prefix, c:suffix) = splitAt p str
        upcaseChars (countChars-1) (prefix ++ toUpper [c] ++ suffix)

cryptoHashText :: Text -> IO Text
cryptoHashText text = do
  strHash <- Crypto.makePassword (BSC.pack $ unpack text) 17
  return $ decodeUtf8 strHash

generatePassword :: Int -> IO (Text, Text)
generatePassword len = do
  passwdStr <- randomMixedCaseString len
  passwdHash <- cryptoHashText $ pack passwdStr
  return (pack passwdStr, passwdHash)

renderUrlToText :: Route App -> Handler Text
renderUrlToText route = do
  renderUrl <- getUrlRender
  return $ renderUrl route

constTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Text -> Field m Text
constTextField myText = Field
    { fieldParse = parseHelper $ \_ -> Right myText
    , fieldView = \theId name attrs _ isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="text" step=1 :isReq:required="" value="#{myText}">
|]
    , fieldEnctype = UrlEncoded
    }

dbSystemUser :: Text
dbSystemUser = "system"

groupEntities :: [(Entity a, Entity b)] -> [(Entity a, [Entity b])]
groupEntities es =
  L.map ((\(es1, es2) -> (L.head es1, es2)) . L.unzip) $
  L.groupBy (\(Entity id1 _, _) (Entity id2 _, _) -> id1 == id2) es

groupEntitiesMaybe :: [(Entity a, Maybe (Entity b))] -> [(Entity a, [Entity b])]
groupEntitiesMaybe es =
  L.map ((\(es1, es2) -> (L.head es1, M.catMaybes es2)) . L.unzip) $
  L.groupBy (\(Entity id1 _, _) (Entity id2 _, _) -> id1 == id2) es

fileBytes :: FileInfo -> Handler B.ByteString
fileBytes fileInfo = do
  bytesL <- runResourceT $ fileSource fileInfo $$ CB.sinkLbs
  return $ toStrict bytesL

humanReadableBytes :: Integer -> String
humanReadableBytes size
  | null pairs = printf "%.0fZiB" (size'/1024^(7::Integer))
  | otherwise  = if unit == "" then printf "%dB" size
                 else printf "%.1f%sB" n unit
  where
    (n, unit):_ = pairs
    pairs = zip (L.iterate (/1024) size') units
    size' = fromIntegral size :: Double
    units = ["","KB","MB","GB","TB","PB","EB","ZB"] :: [String]

getCurrentDay :: IO Day
getCurrentDay = getCurrentTime >>= return . utctDay

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

maybeTextToText :: Maybe Text -> Text
maybeTextToText Nothing = ""
maybeTextToText (Just t) = t

--------------------------------------------------------------------------------
-- format helpers
--------------------------------------------------------------------------------

dayFormatGerman :: String
dayFormatGerman = "%d.%m.%Y"

yearMonthFormatGerman :: String
yearMonthFormatGerman = "%m.%Y"

dayFormatHtml5 :: String
dayFormatHtml5 = "%Y-%m-%d"

formatDay :: Day -> Text
formatDay = pack . formatTime defaultTimeLocale dayFormatGerman

formatYearMonth :: Day -> Text
formatYearMonth = pack . formatTime defaultTimeLocale yearMonthFormatGerman

formatMaybeDay :: Maybe Day -> Text
formatMaybeDay (Just day) = formatDay day
formatMaybeDay _ = ""

formatLocalTime :: TimeZone -> UTCTime -> String
formatLocalTime timeZone utcTime = formatTime defaultTimeLocale "%d.%m.%Y %H:%M:%S" $ utcToLocalTime timeZone utcTime

formatLocalTimeDayPart :: TimeZone -> UTCTime -> String
formatLocalTimeDayPart timeZone utcTime = formatTime defaultTimeLocale "%d.%m.%Y" $ utcToLocalTime timeZone utcTime

formatDouble :: Double -> Text
formatDouble x = T.replace "." "," (pack $ printf "%.2f" x)

formatMaybeDouble :: Maybe Double -> Text
formatMaybeDouble (Just x) = formatDouble x
formatMaybeDouble _ = ""

formatMaybeInt :: Maybe Int -> Text
formatMaybeInt (Just x) = pack $ show x
formatMaybeInt _ = ""

formatEuro :: Double -> Text
formatEuro x = formatDouble x ++ " €"

formatMaybeEuro :: Maybe Double -> Text
formatMaybeEuro (Just x) = formatEuro x
formatMaybeEuro _ = ""

formatDoublePercent :: Double -> Text
formatDoublePercent x = formatDouble x ++ " %"


localizedMsg :: MsgGlobal -> Handler Text
localizedMsg message = do
  master <- getYesod
  langs <- languages
  return $ renderMessage master langs message

-- gen i18n global - start
data MsgGlobal =
  MsgGlobalHome
  | MsgGlobalAdmin
  | MsgGlobalLogout
  | MsgGlobalMyProfile
  | MsgGlobalUsers
  | MsgGlobalAddUser
  | MsgGlobalEditUser
  | MsgGlobalDeleteUser
  | MsgGlobalConfigurations
  | MsgGlobalEditConfig

instance RenderMessage App MsgGlobal where
  renderMessage _ []        = renderGlobalGerman
  renderMessage _ ("de":_) = renderGlobalGerman
  renderMessage _ ("en-US":_) = renderGlobalEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderGlobalGerman :: MsgGlobal -> Text
renderGlobalGerman MsgGlobalHome = "Home"
renderGlobalGerman MsgGlobalAdmin = "Admin"
renderGlobalGerman MsgGlobalLogout = "Logout"
renderGlobalGerman MsgGlobalMyProfile = "Mein profil"
renderGlobalGerman MsgGlobalUsers = "Nutzer"
renderGlobalGerman MsgGlobalAddUser = "Nutzer hinzufügen"
renderGlobalGerman MsgGlobalEditUser = "Nutzer bearbeiten"
renderGlobalGerman MsgGlobalDeleteUser = "Nutzer löschen"
renderGlobalGerman MsgGlobalConfigurations = "Konfigurationen"
renderGlobalGerman MsgGlobalEditConfig = "Konfiguration bearbeiten"

renderGlobalEnglish :: MsgGlobal -> Text
renderGlobalEnglish MsgGlobalHome = "Home"
renderGlobalEnglish MsgGlobalAdmin = "Admin"
renderGlobalEnglish MsgGlobalLogout = "Logout"
renderGlobalEnglish MsgGlobalMyProfile = "My Profile"
renderGlobalEnglish MsgGlobalUsers = "Users"
renderGlobalEnglish MsgGlobalAddUser = "Add user"
renderGlobalEnglish MsgGlobalEditUser = "Edit user"
renderGlobalEnglish MsgGlobalDeleteUser = "Delete user"
renderGlobalEnglish MsgGlobalConfigurations = "Configurations"
renderGlobalEnglish MsgGlobalEditConfig = "Edit config"

-- gen i18n global - end
