{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson

import Generator

main :: IO ()
main = generate context

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
        , bModelAddFormArgs = Nothing
        , bModelEditFormArgs = Nothing
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Nothing
        , bModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddPostExtraStoreFunc = Nothing
        , bModelEditPostExtraStoreFunc = Nothing
        , bModelAddFormTitleMsg = Just "MsgGlobalAddUser"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditUser"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteUser"
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
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
              { bFieldName = "isAdmin"
              , bFieldLabelDe = Just "Ist Admin?"
              , bFieldLabelEn = Just "Is admin?"
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
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
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
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "isResetPassword"
              , bFieldLabelDe = Just "Neues Passwort generieren? (Wird per Email zugesendet)"
              , bFieldLabelEn = Just "Generate new password? (Will be sent by email)"
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
        , bModelAddFormArgs = Nothing
        , bModelEditFormArgs = Nothing
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelDeleteFormDataJsonUrl = Nothing
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddPostExtraStoreFunc = Nothing
        , bModelEditPostExtraStoreFunc = Nothing
        , bModelAddFormTitleMsg = Nothing
        , bModelEditFormTitleMsg = Just "MsgGlobalEditConfig"
        , bModelDeleteFormTitleMsg = Nothing
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
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
        , bModelAddFormArgs = Nothing
        , bModelEditFormArgs = Nothing
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Just "MyprojectR TestMailDataJsonR"
        , bModelDeleteFormDataJsonUrl = Nothing
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddPostExtraStoreFunc = Nothing
        , bModelEditPostExtraStoreFunc = Nothing
        , bModelAddFormTitleMsg = Just "MsgGlobalSendTestMail"
        , bModelEditFormTitleMsg = Nothing
        , bModelDeleteFormTitleMsg = Nothing
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
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
    , BTranslation { bTranslationKey = "language", bTranslationDe = "Sprache", bTranslationEn = "Language" }
    , BTranslation { bTranslationKey = "myProfile", bTranslationDe = "Mein Profil", bTranslationEn = "My Profile" }
    , BTranslation { bTranslationKey = "editMyProfile", bTranslationDe = "Mein Profil bearbeiten", bTranslationEn = "Edit my profile" }
    , BTranslation { bTranslationKey = "reallyDelete", bTranslationDe = "Möchten sie wirklich löschen?", bTranslationEn = "Are you sure to delete?" }
    , BTranslation { bTranslationKey = "users", bTranslationDe = "Nutzer", bTranslationEn = "Users" }
    , BTranslation { bTranslationKey = "addUser", bTranslationDe = "Nutzer hinzufügen", bTranslationEn = "Add user" }
    , BTranslation { bTranslationKey = "editUser", bTranslationDe = "Nutzer bearbeiten", bTranslationEn = "Edit user" }
    , BTranslation { bTranslationKey = "deleteUser", bTranslationDe = "Nutzer löschen", bTranslationEn = "Delete user" }
    , BTranslation { bTranslationKey = "configurations", bTranslationDe = "Konfigurationen", bTranslationEn = "Configurations" }
    , BTranslation { bTranslationKey = "editConfig", bTranslationDe = "Konfiguration bearbeiten", bTranslationEn = "Edit config" }
    , BTranslation { bTranslationKey = "testMail", bTranslationDe = "Test-Mail", bTranslationEn = "Test-Mail" }
    , BTranslation { bTranslationKey = "sendTestMail", bTranslationDe = "Test-Mail senden...", bTranslationEn = "Send Test-Mail..." }
    , BTranslation { bTranslationKey = "cancel", bTranslationDe = "Abbrechen", bTranslationEn = "Cancel" }
    ]
  }
