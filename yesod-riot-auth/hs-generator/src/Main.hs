{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Casing as TC
import Data.Aeson
import qualified Data.Maybe as M
import qualified Data.List as L
import qualified Text.Ginger as G
import qualified System.IO as SIO (IOMode(ReadMode), hSetEncoding, withFile, utf8_bom)
import qualified System.IO.Strict as SIOS
import qualified System.IO.Error as SIOE
import qualified Options.Applicative as O
import Data.Semigroup ((<>))

main :: IO ()
main = do
  templFile <- O.execParser argInfo
  template <- loadTemplate templFile
  putStrLn . Text.unpack $ G.easyRender context template

-- command line parser

argParser :: O.Parser String
argParser = O.argument O.str (O.metavar "TEMPLATE" <> O.help "Ginger template file")

argInfo :: O.ParserInfo String
argInfo = O.info (argParser O.<**> O.helper)
  ( O.fullDesc
  <> O.progDesc "Generate code from TEMPLATE"
  <> O.header "hs-generator - yesod partioal code generator" )

-- ginger

loadTemplate :: String -> IO (G.Template G.SourcePos)
loadTemplate templFile = do
  eitherTemplate <- G.parseGingerFile' opts templFile
  return $ case eitherTemplate of
             Left err -> error . show $ err
             Right template' -> template'

opts :: G.ParserOptions IO
opts = (G.mkParserOptions fileResolver) { G.poSourceName = Nothing
                                        , G.poKeepTrailingNewline = True }

fileResolver :: G.IncludeResolver IO
fileResolver filename = do
  content <- loadFile filename
  return $ Just content

loadFile :: FilePath -> IO String
loadFile fn =
  SIOE.tryIOError (loadFile' $ "ginger/" ++ fn) >>= \e ->
    case e of
      Right contents -> return contents
      Left err -> return $ show err
  where
    loadFile' :: FilePath -> IO String
    loadFile' fn' = do
      SIO.withFile fn' SIO.ReadMode $ \h -> do
        SIO.hSetEncoding h SIO.utf8_bom
        contents <- SIOS.hGetContents h
        return contents

-- helpers

upperFirst :: Text -> Text
upperFirst t = Text.append (Text.toUpper $ Text.take 1 t) (Text.drop 1 t)

lowerFirst :: Text -> Text
lowerFirst t = Text.append (Text.toLower $ Text.take 1 t) (Text.drop 1 t)

-- types

data BContext = BContext
  { bContextModels :: [BModel]
  , bContextTranslations :: [BTranslation]
  }

instance ToJSON BContext where
  toJSON o = object $
    [ "models" .= bContextModels o
    , "translations" .= bContextTranslations o
    ] ++ (map (\bModel@(BModel {bModelName = modelName}) -> (modelName <> "Model") .= bModel) $ bContextModels o)

data BModel = BModel
  { bModelName :: Text
  , bModelLabel :: Text
  , bModelIsJson :: Bool
  , bModelDbUniquenesses :: [Text]
  , bModelDbHasHistoryTable :: Bool
  , bModelHsDerivings :: [Text]
  , bModelFields :: [BField]
  , bModelAddFormEntityLoader :: Maybe Text
  , bModelEditFormEntityLoader :: Maybe Text
  , bModelDeleteFormEntityLoader :: Maybe Text
  , bModelAddFormDataJsonUrl :: Maybe Text
  , bModelEditFormDataJsonUrl :: Maybe Text
  , bModelDeleteFormDataJsonUrl :: Maybe Text
  , bModelAddFormHasDefaultModel :: Bool
  , bModelEditPostLoadsModel :: Bool
  , bModelDeletePostLoadsModel :: Bool
  , bModelParentHsType :: Maybe Text
  }

instance ToJSON BModel where
  toJSON o = object
    [ "name" .= bModelName o
    , "nameCap" .= (upperFirst $ bModelName o)
    , "label" .= bModelLabel o
    , "isJson" .= bModelIsJson o
    , "dbUniquenesses" .= bModelDbUniquenesses o
    , "dbHasHistoryTable" .= bModelDbHasHistoryTable o
    , "dbTableName" .= (TC.toQuietSnake $ TC.fromAny (Text.unpack $ bModelName o))
    , "dbHistoryTableName" .= ((TC.toQuietSnake $ TC.fromAny (Text.unpack $ bModelName o)) ++ "_history")
    , "dbUpdatableFields" .= (filter (\field -> case bFieldDb field of
                                                Just BFieldDb {bFieldDbCanUpdate = canUpdate} -> canUpdate
                                                Nothing -> False
                                   ) $ bModelFields o)
    , "hsDerivings" .= bModelHsDerivings o
    , "fields" .= bModelFields o
    , "addViewFields" .= (filter (\field -> M.isJust $ bFieldAddView field) $ bModelFields o)
    , "editViewFields" .= (filter (\field -> M.isJust $ bFieldEditView field) $ bModelFields o)
    , "isInDb" .= (L.any M.isJust $ L.map bFieldDb $ bModelFields o)
    , "addFormEntityLoader" .= bModelAddFormEntityLoader o
    , "editFormEntityLoader" .= bModelEditFormEntityLoader o
    , "deleteFormEntityLoader" .= bModelDeleteFormEntityLoader o
    , "addFormDataJsonUrl" .= bModelAddFormDataJsonUrl o
    , "editFormDataJsonUrl" .= bModelEditFormDataJsonUrl o
    , "deleteFormDataJsonUrl" .= bModelDeleteFormDataJsonUrl o
    , "addFormHasDefaultModel" .= bModelAddFormHasDefaultModel o
    , "editPostLoadsModel" .= bModelEditPostLoadsModel o
    , "deletePostLoadsModel" .= bModelDeletePostLoadsModel o
    , "parentHsType" .= bModelParentHsType o
    , "parentHsParamId" .= (case bModelParentHsType o of
                              Just parentHsType -> lowerFirst $ Text.append parentHsType "Id"
                              _ -> "")
    , "formHasProgressBar" .= (any (\field -> bFieldHsType field == "FileInfo") $ bModelFields o)
    ]


data BTranslation = BTranslation
  { bTranslationKey :: Text
  , bTranslationDe :: Text
  , bTranslationEn :: Text
  }

instance ToJSON BTranslation where
  toJSON o = object
    [ "key" .= bTranslationKey o
    , "keyCap" .= (upperFirst $ bTranslationKey o)
    , "de" .= bTranslationDe o
    , "en" .= bTranslationEn o
    ]


data BFieldDb = BFieldDb
  { bFieldDbIsNullable :: Bool
  , bFieldDbDefault :: Maybe Text
  , bFieldDbCanUpdate :: Bool
  }

instance ToJSON BFieldDb where
  toJSON o = object
    [ "isNullable" .= bFieldDbIsNullable o
    , "isNotNullable" .= (not $ bFieldDbIsNullable o)
    , "default" .= bFieldDbDefault o
    , "canUpdate" .= bFieldDbCanUpdate o
    ]

data BFieldAddView = BFieldAddView
  { bFieldAddViewIsRequired :: Bool
  , bFieldAddViewIsDisabled :: Bool
  , bFieldAddViewAttrs :: [BFieldAttr]
  , bFieldAddViewDefault :: Maybe Text
  }

instance ToJSON BFieldAddView where
  toJSON o = object
    [ "isRequired" .= bFieldAddViewIsRequired o
    , "isOptional" .= (not $ bFieldAddViewIsRequired o)
    , "isDisabled" .= bFieldAddViewIsDisabled o
    , "isEnabled" .= (not $ bFieldAddViewIsDisabled o)
    , "attrs" .= ((if bFieldAddViewIsDisabled o then [BFieldAttr "disabled" ""] else [])
                   ++ bFieldAddViewAttrs o)
    , "default" .= bFieldAddViewDefault o
    ]

data BFieldEditView = BFieldEditView
  { bFieldEditViewIsRequired :: Bool
  , bFieldEditViewIsDisabled :: Bool
  , bFieldEditViewAttrs :: [BFieldAttr]
  , bFieldEditViewDefault :: Maybe Text
  }

instance ToJSON BFieldEditView where
  toJSON o = object
    [ "isRequired" .= bFieldEditViewIsRequired o
    , "isOptional" .= (not $ bFieldEditViewIsRequired o)
    , "isDisabled" .= bFieldEditViewIsDisabled o
    , "isEnabled" .= (not $ bFieldEditViewIsDisabled o)
    , "attrs" .= ((if bFieldEditViewIsDisabled o then [BFieldAttr "disabled" ""] else [])
                   ++ bFieldEditViewAttrs o)
    , "default" .= bFieldEditViewDefault o
    ]

data BField = BField
  { bFieldName :: Text
  , bFieldLabelDe :: Maybe Text
  , bFieldLabelEn :: Maybe Text
  , bFieldHsType :: Text
  , bFieldDb :: Maybe BFieldDb
  , bFieldFormFieldType :: Maybe Text
  , bFieldAddView :: Maybe BFieldAddView
  , bFieldEditView :: Maybe BFieldEditView
  }

instance ToJSON BField where
  toJSON o = object
    [ "name" .= bFieldName o
    , "nameCap" .= (upperFirst $ bFieldName o)
    , "dbColumnName" .= (TC.toQuietSnake $ TC.fromAny (Text.unpack $ bFieldName o))
    , "labelDe" .= bFieldLabelDe o
    , "labelEn" .= bFieldLabelEn o
    , "hsType" .= bFieldHsType o
    , "db" .= bFieldDb o
    , "formFieldType" .= bFieldFormFieldType o
    , "addView" .= bFieldAddView o
    , "editView" .= bFieldEditView o
    , "isHsTypeBool" .= (bFieldHsType o == "Bool")
    , "isForeignKey" .= ((Text.takeEnd 2 $ bFieldName o) == "Id")
    ]

data BFieldAttr = BFieldAttr
  { bFieldAttrKey :: Text
  , bFieldAttrValue :: Text
  }

instance ToJSON BFieldAttr where
  toJSON o = object
    [ "key" .= bFieldAttrKey o
    , "value" .= bFieldAttrValue o
    ]

-- model context

context :: Value
context =
  toJSON $
  BContext
  { bContextModels =
      [ BModel
        { bModelName = "user"
        , bModelLabel = "User"
        , bModelIsJson = False
        , bModelDbUniquenesses = ["UniqueUser ident"]
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Nothing
        , bModelDeleteFormDataJsonUrl = Just "MyprojectR AdminPageDataJsonR"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelParentHsType = Nothing
        , bModelFields =
            [ BField
              { bFieldName = "ident"
              , bFieldLabelDe = Just "Login"
              , bFieldLabelEn = Just "Login"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "password"
              , bFieldLabelDe = Just "Passwort"
              , bFieldLabelEn = Just "Password"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "email"
              , bFieldLabelDe = Just "Email"
              , bFieldLabelEn = Just "Email"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "isResetPassword"
              , bFieldLabelDe = Just "Neues Passwort generieren?"
              , bFieldLabelEn = Just "Generate new password?"
              , bFieldHsType = "Bool"
              , bFieldDb = Nothing
              , bFieldFormFieldType = Just "checkBoxField"
              , bFieldAddView = Nothing
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldEditViewDefault = Just "Nothing"
                  }
              }
            ]
        }
      , BModel
        { bModelName = "config"
        , bModelLabel = "Config"
        , bModelIsJson = True
        , bModelDbUniquenesses = ["UniqueCode code"]
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Just "MyprojectR AdminPageDataJsonR"
        , bModelDeleteFormDataJsonUrl = Nothing
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelParentHsType = Nothing
        , bModelFields =
            [ BField
              { bFieldName = "code"
              , bFieldLabelDe = Just "Code"
              , bFieldLabelEn = Just "Code"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView = Nothing
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = True
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "stringValue"
              , bFieldLabelDe = Just "String-Wert"
              , bFieldLabelEn = Just "String-Value"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "intValue"
              , bFieldLabelDe = Just "Integer-Wert"
              , bFieldLabelEn = Just "Integer-Value"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "doubleValue"
              , bFieldLabelDe = Just "Double-Wert"
              , bFieldLabelEn = Just "Double-Value"
              , bFieldHsType = "Double"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "doubleField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "boolValue"
              , bFieldLabelDe = Just "Boolean-Wert"
              , bFieldLabelEn = Just "Boolean-Value"
              , bFieldHsType = "Bool"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "checkBoxField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs = []
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }
      , BModel
        { bModelName = "testmail"
        , bModelLabel = "Test Mail"
        , bModelIsJson = False
        , bModelDbUniquenesses = []
        , bModelDbHasHistoryTable = False
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Just "MyprojectR TestMailDataJsonR"
        , bModelDeleteFormDataJsonUrl = Nothing
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelParentHsType = Nothing
        , bModelFields =
            [ BField
              { bFieldName = "email"
              , bFieldLabelDe = Just "Email"
              , bFieldLabelEn = Just "Email"
              , bFieldHsType = "Text"
              , bFieldDb = Nothing
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView = Nothing
              }
            ]
        }
      ]
  , bContextTranslations =
    [ BTranslation { bTranslationKey = "home", bTranslationDe = "Home", bTranslationEn = "Home" }
    , BTranslation { bTranslationKey = "admin", bTranslationDe = "Admin", bTranslationEn = "Admin" }
    , BTranslation { bTranslationKey = "logout", bTranslationDe = "Logout", bTranslationEn = "Logout" }
    , BTranslation { bTranslationKey = "myProfile", bTranslationDe = "Mein profil", bTranslationEn = "My Profile" }
    , BTranslation { bTranslationKey = "users", bTranslationDe = "Nutzer", bTranslationEn = "Users" }
    , BTranslation { bTranslationKey = "addUser", bTranslationDe = "Nutzer hinzufügen", bTranslationEn = "Add user" }
    , BTranslation { bTranslationKey = "editUser", bTranslationDe = "Nutzer bearbeiten", bTranslationEn = "Edit user" }
    , BTranslation { bTranslationKey = "deleteUser", bTranslationDe = "Nutzer löschen", bTranslationEn = "Delete user" }
    , BTranslation { bTranslationKey = "configurations", bTranslationDe = "Konfigurationen", bTranslationEn = "Configurations" }
    , BTranslation { bTranslationKey = "editConfig", bTranslationDe = "Konfiguration bearbeiten", bTranslationEn = "Edit config" }
    , BTranslation { bTranslationKey = "testMail", bTranslationDe = "Test-Mail", bTranslationEn = "Test-Mail" }
    , BTranslation { bTranslationKey = "sendTestMail", bTranslationDe = "Test-Mail senden...", bTranslationEn = "Send Test-Mail..." }
    ]
  }
