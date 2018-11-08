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
  { bContextCrudModels =
      [ BCrudModel
        { bCrudModelName = "user"
        , bCrudModelLabel = "User"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = ["UniqueUser ident"]
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Nothing
        , bCrudModelDeleteFormDataJsonUrl = Just "AdminR AdminDataR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgUserAddUser"
        , bCrudModelEditFormTitleMsg = Just "MsgUserEditUser"
        , bCrudModelDeleteFormTitleMsg = Just "MsgUserDeleteUser"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "ident"
              , bCrudFieldLabelDe = Just "Login"
              , bCrudFieldLabelEn = Just "Login"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "password"
              , bCrudFieldLabelDe = Just "Passwort"
              , bCrudFieldLabelEn = Just "Password"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "email"
              , bCrudFieldLabelDe = Just "Email"
              , bCrudFieldLabelEn = Just "Email"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isAdmin"
              , bCrudFieldLabelDe = Just "Ist Admin?"
              , bCrudFieldLabelEn = Just "Is admin?"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isResetPassword"
              , bCrudFieldLabelDe = Just "Neues Passwort generieren? (Wird per Email zugesendet)"
              , bCrudFieldLabelEn = Just "Generate new password? (Will be sent by email)"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Just "Nothing"
                  }
              }
            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "users", bTranslationDe = "Nutzer", bTranslationEn = "Users" }
          , BTranslation { bTranslationKey = "addUser", bTranslationDe = "Nutzer hinzufügen", bTranslationEn = "Add user" }
          , BTranslation { bTranslationKey = "editUser", bTranslationDe = "Nutzer bearbeiten", bTranslationEn = "Edit user" }
          , BTranslation { bTranslationKey = "deleteUser", bTranslationDe = "Nutzer löschen", bTranslationEn = "Delete user" }
          ]
        }
      , BCrudModel
        { bCrudModelName = "config"
        , bCrudModelLabel = "Config"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = ["UniqueCode code"]
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Just "AdminR AdminDataR"
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Nothing
        , bCrudModelEditFormTitleMsg = Just "MsgConfigEditConfig"
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "code"
              , bCrudFieldLabelDe = Just "Code"
              , bCrudFieldLabelEn = Just "Code"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = True
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "stringValue"
              , bCrudFieldLabelDe = Just "String-Wert"
              , bCrudFieldLabelEn = Just "String-Value"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "intValue"
              , bCrudFieldLabelDe = Just "Integer-Wert"
              , bCrudFieldLabelEn = Just "Integer-Value"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "doubleValue"
              , bCrudFieldLabelDe = Just "Double-Wert"
              , bCrudFieldLabelEn = Just "Double-Value"
              , bCrudFieldHsType = "Double"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "doubleField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "boolValue"
              , bCrudFieldLabelDe = Just "Boolean-Wert"
              , bCrudFieldLabelEn = Just "Boolean-Value"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "configurations", bTranslationDe = "Konfigurationen", bTranslationEn = "Configurations" }
          , BTranslation { bTranslationKey = "editConfig", bTranslationDe = "Konfiguration bearbeiten", bTranslationEn = "Edit config" }
          ]
        }
      , BCrudModel
        { bCrudModelName = "testmail"
        , bCrudModelLabel = "Test Mail"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = False
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Just "MyprojectR TestMailDataJsonR"
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgTestmailSendTestMail"
        , bCrudModelEditFormTitleMsg = Nothing
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "email"
              , bCrudFieldLabelDe = Just "Email"
              , bCrudFieldLabelEn = Just "Email"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView = Nothing
              }
            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "testMail", bTranslationDe = "Test-Mail", bTranslationEn = "Test-Mail" }
          , BTranslation { bTranslationKey = "sendTestMail", bTranslationDe = "Test-Mail senden...", bTranslationEn = "Send Test-Mail..." }
          ]
        }
      ]


  , bContextActionModels =
      [
      ]


  , bContextGlobalTranslations =
    [ BTranslation { bTranslationKey = "home", bTranslationDe = "Home", bTranslationEn = "Home" }
    , BTranslation { bTranslationKey = "admin", bTranslationDe = "Admin", bTranslationEn = "Admin" }
    , BTranslation { bTranslationKey = "logout", bTranslationDe = "Logout", bTranslationEn = "Logout" }
    , BTranslation { bTranslationKey = "language", bTranslationDe = "Sprache", bTranslationEn = "Language" }
    , BTranslation { bTranslationKey = "myProfile", bTranslationDe = "Mein Profil", bTranslationEn = "My Profile" }
    , BTranslation { bTranslationKey = "editMyProfile", bTranslationDe = "Mein Profil bearbeiten", bTranslationEn = "Edit my profile" }
    , BTranslation { bTranslationKey = "reallyDelete", bTranslationDe = "Möchten sie wirklich löschen?", bTranslationEn = "Are you sure to delete?" }
    , BTranslation { bTranslationKey = "cancel", bTranslationDe = "Abbrechen", bTranslationEn = "Cancel" }
    ]
  }
