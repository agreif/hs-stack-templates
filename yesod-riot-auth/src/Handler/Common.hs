{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Common where

import qualified Control.Monad.Random as R
import qualified Crypto.PasswordStore as Crypto
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as C
import qualified Data.Conduit.Binary as CB
import Data.FileEmbed (embedFile)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time
import Import
import qualified Text.Printf as PF

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/x-icon" $
    toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR =
  return $ TypedContent typePlain $
    toContent $(embedFile "config/robots.txt")

newtype VPostSubmitSuccess = VPostSubmitSuccess
  { fsPostSuccessDataJsonUrl :: Text
  }

instance ToJSON VPostSubmitSuccess where
  toJSON o =
    object
      [ "dataJsonUrl" .= fsPostSuccessDataJsonUrl o
      ]

newtype VFormSubmitSuccess = VFormSubmitSuccess
  { fsSuccessDataJsonUrl :: Text
  }

instance ToJSON VFormSubmitSuccess where
  toJSON o =
    object
      [ "isFormValid" .= True,
        "dataJsonUrl" .= fsSuccessDataJsonUrl o
      ]

newtype VFormSubmitInvalid = VFormSubmitInvalid
  { fsInvalidModalWidgetHtml :: Text
  }

instance ToJSON VFormSubmitInvalid where
  toJSON o =
    object
      [ "modalWidgetHtml" .= fsInvalidModalWidgetHtml o
      ]

newtype VFormSubmitStale = VFormSubmitStale
  { fsStaleDataJsonUrl :: Text
  }

instance ToJSON VFormSubmitStale where
  toJSON o =
    object
      [ "isStaleObjectState" .= True,
        "dataJsonUrl" .= fsStaleDataJsonUrl o
      ]

data JData = JData
  { jDataAppName :: Text,
    jDataUserIdent :: Text,
    jDataMainNavItems :: [JDataNavItem],
    jDataSubNavItems :: [JDataNavItem],
    jDataPages :: JDataPages,
    jDataHistoryState :: Maybe JDataHistoryState,
    jDataCsrfToken :: Maybe Text,
    jDataCsrfHeaderName :: Text,
    jDataCsrfParamName :: Text,
    jDataBreadcrumbItems :: [JDataBreadcrumbItem],
    jDataCurrentLanguage :: Language,
    jDataTranslation :: Translation,
    jDataLanguageDeUrl :: Text,
    jDataLanguageEnUrl :: Text
  }

instance ToJSON JData where
  toJSON o =
    object
      [ "appName" .= jDataAppName o,
        "userIdent" .= jDataUserIdent o,
        "mainNavItems" .= jDataMainNavItems o,
        "subNavItems" .= jDataSubNavItems o,
        "pages" .= jDataPages o,
        "historyState" .= jDataHistoryState o,
        "csrfHeaderName" .= jDataCsrfHeaderName o,
        "csrfParamName" .= jDataCsrfParamName o,
        "csrfToken" .= jDataCsrfToken o,
        "breadcrumbItems" .= jDataBreadcrumbItems o,
        "currentLanguage" .= jDataCurrentLanguage o,
        "translation" .= jDataTranslation o,
        "languageDeUrl" .= jDataLanguageDeUrl o,
        "languageEnUrl" .= jDataLanguageEnUrl o
      ]

data JDataNavItem = JDataNavItem
  { jDataNavItemId :: Maybe Text,
    jDataNavItemLabel :: Text,
    jDataNavItemIsActive :: Bool,
    jDataNavItemUrl :: Maybe Text,
    jDataNavItemDataUrl :: Maybe Text,
    jDataNavItemBadge :: Maybe Text,
    jDataNavItemDropdownItems :: Maybe [JDataNavItem]
  }

instance ToJSON JDataNavItem where
  toJSON o =
    object
      [ "id" .= jDataNavItemId o,
        "label" .= jDataNavItemLabel o,
        "isActive" .= jDataNavItemIsActive o,
        "url" .= jDataNavItemUrl o,
        "dataUrl" .= jDataNavItemDataUrl o,
        "badge" .= jDataNavItemBadge o,
        "dropdownItems" .= jDataNavItemDropdownItems o
      ]

data JDataBreadcrumbItem = JDataBreadcrumbItem
  { jDataBreadcrumbItemLabel :: Text,
    jDataBreadcrumbItemDataUrl :: Text
  }

instance ToJSON JDataBreadcrumbItem where
  toJSON o =
    object
      [ "label" .= jDataBreadcrumbItemLabel o,
        "dataUrl" .= jDataBreadcrumbItemDataUrl o
      ]

data JDataHistoryState = JDataHistoryState
  { jDataHistoryStateUrl :: Text,
    jDataHistoryStateTitle :: Text
  }

instance ToJSON JDataHistoryState where
  toJSON o =
    object
      [ "url" .= jDataHistoryStateUrl o,
        "title" .= jDataHistoryStateTitle o
      ]

data JDataPaginationItem = JDataPaginationItem
  { jDataPaginationItemLabel :: Maybe Text,
    jDataPaginationItemDataUrl :: Maybe Text,
    jDataPaginationItemIsActive :: Bool,
    jDataPaginationItemIsDisabled :: Bool,
    jDataPaginationItemIsPrevious :: Bool,
    jDataPaginationItemIsNext :: Bool
  }

instance ToJSON JDataPaginationItem where
  toJSON o =
    object
      [ "label" .= jDataPaginationItemLabel o,
        "dataUrl" .= jDataPaginationItemDataUrl o,
        "isActive" .= jDataPaginationItemIsActive o,
        "isDisabled" .= jDataPaginationItemIsDisabled o,
        "isPrevious" .= jDataPaginationItemIsPrevious o,
        "isNext" .= jDataPaginationItemIsNext o
      ]

instance ToJSON User where
  toJSON o =
    object
      [ "ident" .= userIdent o,
        "email" .= userEmail o,
        "isAdmin" .= userIsAdmin o
      ]

data JDataPages = JDataPages
  { jDataPageHome :: Maybe JDataPageHome,
    jDataPageAdmin :: Maybe JDataPageAdmin,
    jDataPageDemoaList :: Maybe JDataPageDemoaList,
    jDataPageDemobList :: Maybe JDataPageDemobList,
    jDataPageDemobDetail :: Maybe JDataPageDemobDetail
  }

instance ToJSON JDataPages where
  toJSON o =
    object
      [ "home" .= jDataPageHome o,
        "admin" .= jDataPageAdmin o,
        "demoaList" .= jDataPageDemoaList o,
        "demobList" .= jDataPageDemobList o,
        "demobDetail" .= jDataPageDemobDetail o
      ]

defaultDataPages :: JDataPages
defaultDataPages =
  JDataPages
    { jDataPageHome = Nothing,
      jDataPageAdmin = Nothing,
      jDataPageDemoaList = Nothing,
      jDataPageDemobList = Nothing,
      jDataPageDemobDetail = Nothing
    }

data JDataPageHome = JDataPageHome
  { jDataPageHomeContent :: Text
  }

instance ToJSON JDataPageHome where
  toJSON o =
    object
      [ "content" .= jDataPageHomeContent o
      ]

data JDataPageAdmin = JDataPageAdmin
  { jDataPageAdminUsers :: [JDataUser],
    jDataPageAdminConfigs :: [JDataConfig],
    jDataPageAdminGitCommitDate :: String,
    jDataPageAdminGitCommitMessage :: String,
    jDataPageAdminGitCommitHash :: String,
    jDataPageAdminGitCommitBranch :: String
  }

instance ToJSON JDataPageAdmin where
  toJSON o =
    object
      [ "users" .= jDataPageAdminUsers o,
        "configs" .= jDataPageAdminConfigs o,
        "gitCommitDate" .= jDataPageAdminGitCommitDate o,
        "gitCommitMessage" .= jDataPageAdminGitCommitMessage o,
        "gitCommitHash" .= jDataPageAdminGitCommitHash o,
        "gitCommitBranch" .= jDataPageAdminGitCommitBranch o
      ]

data JDataUser = JDataUser
  { jDataUserEnt :: Entity User,
    jDataUserEditFormUrl :: Text,
    jDataUserDeleteFormUrl :: Text
  }

instance ToJSON JDataUser where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataUserEnt o),
        "editFormUrl" .= jDataUserEditFormUrl o,
        "deleteFormUrl" .= jDataUserDeleteFormUrl o
      ]

data JDataConfig = JDataConfig
  { jDataConfigEnt :: Entity Config,
    jDataConfigEditFormUrl :: Text
  }

instance ToJSON JDataConfig where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataConfigEnt o),
        "editFormUrl" .= jDataConfigEditFormUrl o
      ]

data JDataPageDemoaList = JDataPageDemoaList
  { jDataPageDemoaListDemoas :: [JDataDemoa],
    jDataPageDemoaListAddFormUrl :: Text,
    jDataPageDemoaListPaginationItems :: Maybe [JDataPaginationItem]
  }

instance ToJSON JDataPageDemoaList where
  toJSON o =
    object
      [ "demoas" .= jDataPageDemoaListDemoas o,
        "addFormUrl" .= jDataPageDemoaListAddFormUrl o,
        "paginationItems" .= jDataPageDemoaListPaginationItems o
      ]

data JDataDemoa = JDataDemoa
  { jDataDemoaEnt :: Entity Demoa,
    jDataDemoaEditFormUrl :: Text,
    jDataDemoaDeleteFormUrl :: Text
  }

instance ToJSON JDataDemoa where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataDemoaEnt o),
        "editFormUrl" .= jDataDemoaEditFormUrl o,
        "deleteFormUrl" .= jDataDemoaDeleteFormUrl o
      ]

data JDataPageDemobList = JDataPageDemobList
  { jDataPageDemobListDemobs :: [JDataDemob],
    jDataPageDemobListAddFormUrl :: Text,
    jDataPageDemobListPaginationItems :: Maybe [JDataPaginationItem]
  }

instance ToJSON JDataPageDemobList where
  toJSON o =
    object
      [ "demobs" .= jDataPageDemobListDemobs o
      ]

data JDataDemob = JDataDemob
  { jDataDemobEnt :: Entity Demob,
    jDataDemobDetailUrl :: Text,
    jDataDemobDetailDataUrl :: Text,
    jDataDemobDeleteFormUrl :: Text
  }

instance ToJSON JDataDemob where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataDemobEnt o),
        "detailUrl" .= jDataDemobDetailUrl o,
        "detailDataUrl" .= jDataDemobDetailDataUrl o,
        "deleteFormUrl" .= jDataDemobDeleteFormUrl o
      ]

data JDataPageDemobDetail = JDataPageDemobDetail
  { jDataPageDemobDetailDemobEnt :: Entity Demob,
    jDataPageDemobDetailDemocs :: [JDataDemoc],
    jDataPageDemobDetailDemobEditFormUrl :: Text,
    jDataPageDemobDetailDemocAddFormUrl :: Text
  }

instance ToJSON JDataPageDemobDetail where
  toJSON o =
    object
      [ "demobEnt" .= jDataPageDemobDetailDemobEnt o,
        "democs" .= jDataPageDemobDetailDemocs o,
        "demobEditFormUrl" .= jDataPageDemobDetailDemobEditFormUrl o,
        "democAddFormUrl" .= jDataPageDemobDetailDemocAddFormUrl o
      ]

data JDataDemoc = JDataDemoc
  { jDataDemocEnt :: Entity Democ,
    jDataDemocEditFormUrl :: Text,
    jDataDemocDeleteFormUrl :: Text
  }

instance ToJSON JDataDemoc where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataDemocEnt o),
        "editFormUrl" .= jDataDemocEditFormUrl o,
        "deleteFormUrl" .= jDataDemocDeleteFormUrl o
      ]

--------------------------------------------------------------------------------
-- navigation helpers
--------------------------------------------------------------------------------

data MainNav
  = MainNavHome
  | MainNavAdmin
  | MainNavDemoa
  | MainNavDemob
  deriving (Eq)

mainNavData :: User -> MainNav -> Handler [JDataNavItem]
mainNavData user mainNav = do
  urlRenderer <- getUrlRender
  msgHome <- localizedMsg MsgGlobalHome
  msgAdmin <- localizedMsg MsgGlobalAdmin
  msgDemoas <- localizedMsg MsgDemoaDemoas
  msgDemobs <- localizedMsg MsgDemobDemobs
  return $
    [ JDataNavItem
        { jDataNavItemId = Nothing,
          jDataNavItemLabel = msgHome,
          jDataNavItemIsActive = mainNav == MainNavHome,
          jDataNavItemUrl = Just $ urlRenderer $ MyprojectR MyprojectHomeR,
          jDataNavItemDataUrl = Just $ urlRenderer $ MyprojectR HomeDataR,
          jDataNavItemBadge = Nothing,
          jDataNavItemDropdownItems = Nothing
        }
    ]
      ++ [ JDataNavItem
             { jDataNavItemId = Nothing,
               jDataNavItemLabel = msgAdmin,
               jDataNavItemIsActive = mainNav == MainNavAdmin,
               jDataNavItemUrl = Just $ urlRenderer $ AdminR AdminHomeR,
               jDataNavItemDataUrl = Just $ urlRenderer $ AdminR AdminDataR,
               jDataNavItemBadge = Nothing,
               jDataNavItemDropdownItems = Nothing
             }
           | userIsAdmin user
         ]
      ++ [ JDataNavItem
             { jDataNavItemId = Nothing,
               jDataNavItemLabel = msgDemoas,
               jDataNavItemIsActive = mainNav == MainNavDemoa,
               jDataNavItemUrl = Just $ urlRenderer $ MyprojectR DemoaListR,
               jDataNavItemDataUrl = Just $ urlRenderer $ MyprojectR DemoaListDataR,
               jDataNavItemBadge = Nothing,
               jDataNavItemDropdownItems = Nothing
             }
         ]
      ++ [ JDataNavItem
             { jDataNavItemId = Nothing,
               jDataNavItemLabel = msgDemobs,
               jDataNavItemIsActive = mainNav == MainNavDemob,
               jDataNavItemUrl = Just $ urlRenderer $ MyprojectR DemobListR,
               jDataNavItemDataUrl = Just $ urlRenderer $ MyprojectR DemobListDataR,
               jDataNavItemBadge = Nothing,
               jDataNavItemDropdownItems = Nothing
             }
         ]

--------------------------------------------------------------------------------
-- pagination helpers
--------------------------------------------------------------------------------

getPaginationJDatas :: Int -> Int -> Int -> Int -> (Int -> Route App) -> Handler (Maybe [JDataPaginationItem])
getPaginationJDatas allCount pageSize curPageNum visibleNumsCount' routeFunc = do
  urlRenderer <- getUrlRender
  let visibleNumsCount = if mod visibleNumsCount' 2 == 0 then visibleNumsCount' + 1 else visibleNumsCount'
  let pageCount' = div allCount pageSize
  let pageCount = if mod allCount pageSize == 0 then pageCount' else pageCount' + 1
  (firstPageNum, lastPageNum) <-
    if visibleNumsCount >= pageCount
      then return (1, pageCount) -- show all
      else do
        let (firstNum, lastNum) =
              ( curPageNum - div visibleNumsCount 2,
                curPageNum + div visibleNumsCount 2
              )
        let (firstNum', lastNum') = if firstNum < 1 then (1, visibleNumsCount) else (firstNum, lastNum)
        let (firstNum'', lastNum'') =
              if lastNum > pageCount
                then (pageCount - visibleNumsCount + 1, pageCount)
                else (firstNum', lastNum')
        return (firstNum'', lastNum'')
  let pageNums = [firstPageNum .. lastPageNum]
  case pageCount of
    0 -> return Nothing
    1 -> return Nothing
    _ ->
      return
        $ Just
        $ ( if curPageNum == 1
              then []
              else
                [ JDataPaginationItem
                    { jDataPaginationItemLabel = Nothing,
                      jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc $ curPageNum - 1,
                      jDataPaginationItemIsActive = False,
                      jDataPaginationItemIsDisabled = False,
                      jDataPaginationItemIsPrevious = True,
                      jDataPaginationItemIsNext = False
                    }
                ]
                  ++ ( if firstPageNum == 1
                         then []
                         else
                           [ JDataPaginationItem
                               { jDataPaginationItemLabel = Just "1",
                                 jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc 1,
                                 jDataPaginationItemIsActive = False,
                                 jDataPaginationItemIsDisabled = False,
                                 jDataPaginationItemIsPrevious = False,
                                 jDataPaginationItemIsNext = False
                               }
                           ]
                     )
                  ++ ( if firstPageNum <= 2
                         then []
                         else
                           [ JDataPaginationItem
                               { jDataPaginationItemLabel = Just "...",
                                 jDataPaginationItemDataUrl = Nothing,
                                 jDataPaginationItemIsActive = False,
                                 jDataPaginationItemIsDisabled = True,
                                 jDataPaginationItemIsPrevious = False,
                                 jDataPaginationItemIsNext = False
                               }
                           ]
                     )
          )
          ++ map
            ( \i ->
                JDataPaginationItem
                  { jDataPaginationItemLabel = Just $ formatInt i,
                    jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc $ fromIntegral i,
                    jDataPaginationItemIsActive = i == curPageNum,
                    jDataPaginationItemIsDisabled = False,
                    jDataPaginationItemIsPrevious = False,
                    jDataPaginationItemIsNext = False
                  }
            )
            pageNums
          ++ ( if lastPageNum >= pageCount -1
                 then []
                 else
                   [ JDataPaginationItem
                       { jDataPaginationItemLabel = Just "...",
                         jDataPaginationItemDataUrl = Nothing,
                         jDataPaginationItemIsActive = False,
                         jDataPaginationItemIsDisabled = True,
                         jDataPaginationItemIsPrevious = False,
                         jDataPaginationItemIsNext = False
                       }
                   ]
             )
          ++ ( if lastPageNum == pageCount
                 then []
                 else
                   [ JDataPaginationItem
                       { jDataPaginationItemLabel = Just $ formatInt pageCount,
                         jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc pageCount,
                         jDataPaginationItemIsActive = False,
                         jDataPaginationItemIsDisabled = False,
                         jDataPaginationItemIsPrevious = False,
                         jDataPaginationItemIsNext = False
                       }
                   ]
             )
          ++ if curPageNum == pageCount
            then []
            else
              [ JDataPaginationItem
                  { jDataPaginationItemLabel = Nothing,
                    jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc $ curPageNum + 1,
                    jDataPaginationItemIsActive = False,
                    jDataPaginationItemIsDisabled = False,
                    jDataPaginationItemIsPrevious = False,
                    jDataPaginationItemIsNext = True
                  }
              ]

--------------------------------------------------------------------------------
-- form helpers
--------------------------------------------------------------------------------

strippedTextField :: Field (HandlerFor App) Text
strippedTextField = checkM strippedText textField
  where
    strippedText :: Text -> Handler (Either AppMessage Text)
    strippedText text = return $ Right $ T.strip text

strippedTextareaField :: Field (HandlerFor App) Textarea
strippedTextareaField = checkM strippedText textareaField
  where
    strippedText :: Textarea -> Handler (Either AppMessage Textarea)
    strippedText (Textarea text) = return $ Right $ Textarea $ T.strip text

verticalScrollingCheckboxesField ::
  (YesodPersist site, RenderMessage site FormMessage, YesodPersistBackend site ~ SqlBackend, Eq a) =>
  HandlerFor site (OptionList a) ->
  Field (HandlerFor site) [a]
verticalScrollingCheckboxesField ioptlist =
  (multiSelectField ioptlist)
    { fieldView = \theId name attrs val _isReq -> do
        opts <- olOptions <$> handlerToWidget ioptlist
        let optselected (Left _) _ = False
            optselected (Right vals) opt = optionInternalValue opt `elem` vals
        [whamlet|
                <div ##{theId} style="border:1px solid #ccc; height: 450px; overflow-y: scroll;">
                  $forall opt <- opts
                    <label style="display: block;">
                      <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                      #{optionDisplay opt}
                |]
    }

verticalCheckboxesField ::
  (YesodPersist site, RenderMessage site FormMessage, YesodPersistBackend site ~ SqlBackend, Eq a) =>
  HandlerFor site (OptionList a) ->
  Field (HandlerFor site) [a]
verticalCheckboxesField ioptlist =
  (multiSelectField ioptlist)
    { fieldView = \theId name attrs val _isReq -> do
        opts <- olOptions <$> handlerToWidget ioptlist
        let optselected (Left _) _ = False
            optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
        [whamlet|
              <span ##{theId}>
                $forall opt <- opts
                  <label style="display: block;">
                    <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                    #{optionDisplay opt}
              |]
    }

--------------------------------------------------------------------------------
-- app specific helpers
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- generic helpers
--------------------------------------------------------------------------------

rnd :: (R.RandomGen g) => Int -> Int -> R.Rand g Int
rnd x y = R.getRandomR (x, y)

randomMixedCaseString :: Int -> IO String
randomMixedCaseString len = do
  values <- R.evalRandIO (replicateM len $ rnd 65 90)
  let str = toLower $ map C.chr values
  -- in average in every 2 chars is an uppercase char
  upcaseChars (quot len 2) str
  where
    upcaseChars :: Int -> String -> IO String
    upcaseChars countChars str =
      if countChars == 0
        then return str
        else do
          p <- R.evalRandIO $ rnd 0 (length str - 2)
          let (prefix, c : suffix) = splitAt p str
          upcaseChars (countChars -1) (prefix ++ toUpper [c] ++ suffix)

randomMixedCaseText :: Int -> IO Text
randomMixedCaseText len = do
  str <- randomMixedCaseString len
  return $ pack str

cryptoHashText :: Text -> IO Text
cryptoHashText text = do
  strHash <- Crypto.makePassword (BSC.pack $ unpack text) 17
  return $ decodeUtf8 strHash

generatePassword :: Int -> IO (Text, Text)
generatePassword len = do
  passwdStr' <- randomMixedCaseString len
  let passwdStr = T.replace "i" "2" $ T.replace "I" "3" $ T.replace "l" "4" $ T.replace "L" "5" $ T.replace "o" "6" $ T.replace "O" "7" $ T.replace "0" "8" $ T.replace "1" "9" $ pack passwdStr'
  passwdHash <- cryptoHashText passwdStr
  return (passwdStr, passwdHash)

renderUrlToText :: Route App -> Handler Text
renderUrlToText route = do
  renderUrl <- getUrlRender
  return $ renderUrl route

constTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Text -> Field m Text
constTextField myText =
  Field
    { fieldParse = parseHelper $ \_ -> Right myText,
      fieldView = \theId name attrs _ isReq ->
        toWidget
          [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="text" step=1 :isReq:required="" value="#{myText}">
|],
      fieldEnctype = UrlEncoded
    }

dbSystemUser :: Text
dbSystemUser = "system"

-- groupEntities :: [(Entity a, Entity b)] -> [(Entity a, [Entity b])]
-- groupEntities es =
--   L.map ((\(es1, es2) -> (L.head es1, es2)) . L.unzip) $
--   L.groupBy (\(Entity id1 _, _) (Entity id2 _, _) -> id1 == id2) es

-- groupEntitiesMaybe :: [(Entity a, Maybe (Entity b))] -> [(Entity a, [Entity b])]
-- groupEntitiesMaybe es =
--   L.map ((\(es1, es2) -> (L.head es1, M.catMaybes es2)) . L.unzip) $
--   L.groupBy (\(Entity id1 _, _) (Entity id2 _, _) -> id1 == id2) es

fileBytes :: FileInfo -> Handler B.ByteString
fileBytes fileInfo = do
  bytesL <- runConduit $ fileSource fileInfo .| CB.sinkLbs
  return $ toStrict bytesL

humanReadableBytes :: Integer -> String
humanReadableBytes size
  | null pairs = PF.printf "%.0fZiB" (size' / 1024 ^ (7 :: Integer))
  | otherwise =
    if unit == ""
      then PF.printf "%dB" size
      else PF.printf "%.1f%sB" n unit
  where
    (n, unit) : _ = pairs
    pairs = zip (L.iterate (/ 1024) size') units
    size' = fromIntegral size :: Double
    units = ["", "KB", "MB", "GB", "TB", "PB", "EB", "ZB"] :: [String]

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> getCurrentTime

getCurrentYear :: IO Integer
getCurrentYear = do
  d <- getCurrentDay
  let (year, _, _) = toGregorian d
  return year

maybeTextToText :: Maybe Text -> Text
maybeTextToText Nothing = ""
maybeTextToText (Just t) = t

typeCsv :: ContentType
typeCsv = "text/csv"

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
formatDouble x = T.replace "." "," (pack $ PF.printf "%.2f" x)

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

formatInt :: Int -> Text
formatInt = pack . show

formatInt4Digits :: Int -> String
formatInt4Digits = PF.printf "%04d"

formatMinuteValue :: Int -> Text
formatMinuteValue minVal = pack $ h1 : h2 : ':' : m1 : m2
  where
    (h1 : h2 : m1 : m2) = formatInt4Digits minVal

maybePhone :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text
maybePhone Nothing Nothing Nothing Nothing = Nothing
maybePhone Nothing a n Nothing = Just $ formatMaybeInt a <> "/" <> formatMaybeInt n
maybePhone c a n Nothing = Just $ "+" <> formatMaybeInt c <> " " <> formatMaybeInt a <> "/" <> formatMaybeInt n
maybePhone Nothing a n s = Just $ formatMaybeInt a <> "/" <> formatMaybeInt n <> "-" <> formatMaybeInt s
maybePhone c a n s = Just $ "+" <> formatMaybeInt c <> " " <> formatMaybeInt a <> "/" <> formatMaybeInt n <> "-" <> formatMaybeInt s

maybeMobile :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text
maybeMobile Nothing Nothing Nothing = Nothing
maybeMobile Nothing a n = Just $ formatMaybeInt a <> "/" <> formatMaybeInt n
maybeMobile c a n = Just $ "+" <> formatMaybeInt c <> " " <> formatMaybeInt a <> "/" <> formatMaybeInt n

data Language = DE | EN
  deriving (Generic)

instance ToJSON Language

lookupSessionLanguage :: Handler (Maybe Language)
lookupSessionLanguage = do
  session <- getSession
  case lookup "_LANG" session of
    Just sessionLanguage -> case sessionLanguage of
      "en" -> return $ Just EN
      "en-US" -> return $ Just EN
      _ -> return $ Just DE
    _ -> return Nothing

getLanguage :: Handler Language
getLanguage = do
  maybeLanguage <- lookupSessionLanguage
  case maybeLanguage of
    Just language -> return language
    _ -> do
      langs <- languages
      return $ case langs of
        "en" : _ -> EN
        "en-US" : _ -> EN
        _ -> DE

getTranslation :: Handler Translation
getTranslation = do
  lang <- getLanguage
  return $ case lang of
    EN -> translationEn
    DE -> translationDe

localizedMsg :: AppMessage -> Handler Text
localizedMsg message = do
  master <- getYesod
  language <- getLanguage
  let langs = case language of
        EN -> ["en-US"]
        DE -> ["de"]
  return $ renderMessage master langs message
