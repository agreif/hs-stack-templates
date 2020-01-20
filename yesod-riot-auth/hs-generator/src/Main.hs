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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-medium"
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
                            "uk-input uk-form-small uk-form-width-medium"
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
                            "uk-input uk-form-small uk-form-width-medium"
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
                            "uk-input uk-form-small uk-form-width-medium"
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
        { bCrudModelName = "demoa"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "MyprojectR DemoaListDataR"
        , bCrudModelEditFormDataJsonUrl = Just "MyprojectR $ DemoaListDataR"
        , bCrudModelDeleteFormDataJsonUrl = Just "MyprojectR $ DemoaListDataR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgDemoaAddDemoa"
        , bCrudModelEditFormTitleMsg = Just "MsgDemoaEditDemoa"
        , bCrudModelDeleteFormTitleMsg = Just "MsgDemoaDeleteDemoa"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "MyprojectR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "myattr"
              , bCrudFieldLabelDe = Just "My attribute"
              , bCrudFieldLabelEn = Just "Mein Attribut"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }

            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "demoa", bTranslationDe = "Demoa", bTranslationEn = "Demoa" }
          , BTranslation { bTranslationKey = "demoas", bTranslationDe = "Demoas", bTranslationEn = "Demoas" }
          , BTranslation { bTranslationKey = "addDemoa", bTranslationDe = "Demoa hinzufügen", bTranslationEn = "Add demoa" }
          , BTranslation { bTranslationKey = "editDemoa", bTranslationDe = "Demoa bearbeiten", bTranslationEn = "Edit demoa" }
          , BTranslation { bTranslationKey = "deleteDemoa", bTranslationDe = "Demoa löschen", bTranslationEn = "Delete demoa" }
          ]
        }

      , BCrudModel
        { bCrudModelName = "demob"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "MyprojectR DemobListDataR"
        , bCrudModelEditFormDataJsonUrl = Just "MyprojectR $ DemobDetailDataR demobId"
        , bCrudModelDeleteFormDataJsonUrl = Just "MyprojectR $ DemobListDataR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgDemobAddDemob"
        , bCrudModelEditFormTitleMsg = Just "MsgDemobEditDemob"
        , bCrudModelDeleteFormTitleMsg = Just "MsgDemobDeleteDemob"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "MyprojectR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "myattr"
              , bCrudFieldLabelDe = Just "My attribute"
              , bCrudFieldLabelEn = Just "Mein Attribut"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }

            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "demob", bTranslationDe = "Demob", bTranslationEn = "Demob" }
          , BTranslation { bTranslationKey = "demobs", bTranslationDe = "Demobs", bTranslationEn = "Demobs" }
          , BTranslation { bTranslationKey = "addDemob", bTranslationDe = "Demob hinzufügen", bTranslationEn = "Add demob" }
          , BTranslation { bTranslationKey = "editDemob", bTranslationDe = "Demob bearbeiten", bTranslationEn = "Edit demob" }
          , BTranslation { bTranslationKey = "deleteDemob", bTranslationDe = "Demob löschen", bTranslationEn = "Delete demob" }
          ]
        }






      , BCrudModel
        { bCrudModelName = "democ"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "MyprojectR $ DemobDetailDataR demobId"
        , bCrudModelEditFormDataJsonUrl = Just "MyprojectR $ DemobDetailDataR $ democDemobId democ"
        , bCrudModelDeleteFormDataJsonUrl = Just "MyprojectR $ DemobDetailDataR $ democDemobId democ"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = True
        , bCrudModelDeletePostLoadsModel = True
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgDemocAddDemoc"
        , bCrudModelEditFormTitleMsg = Just "MsgDemocEditDemoc"
        , bCrudModelDeleteFormTitleMsg = Just "MsgDemocDeleteDemoc"
        , bCrudModelParentHsType = Just "Demob"
        , bCrudModelFormRouteHsType = "MyprojectR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "demobId"
              , bCrudFieldLabelDe = Nothing
              , bCrudFieldLabelEn = Nothing
              , bCrudFieldHsType = "DemobId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "myattr"
              , bCrudFieldLabelDe = Just "My attribute"
              , bCrudFieldLabelEn = Just "Mein Attribut"
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
                            "uk-input uk-form-small uk-form-width-large"
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
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }

            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "democ", bTranslationDe = "Democ", bTranslationEn = "Democ" }
          , BTranslation { bTranslationKey = "democs", bTranslationDe = "Democs", bTranslationEn = "Democs" }
          , BTranslation { bTranslationKey = "addDemoc", bTranslationDe = "Democ hinzufügen", bTranslationEn = "Add democ" }
          , BTranslation { bTranslationKey = "editDemoc", bTranslationDe = "Democ bearbeiten", bTranslationEn = "Edit democ" }
          , BTranslation { bTranslationKey = "deleteDemoc", bTranslationDe = "Democ löschen", bTranslationEn = "Delete democ" }
          ]
        }




      ]

  , bContextActionModels =
      [ BActionModel
        { bActionModelName = "testmail"
        , bActionModelAction = "send"
        , bActionModelFormArgs = Nothing
        , bActionModelFormEntityLoader = Nothing
        , bActionModelFormDataJsonUrl = Nothing
        , bActionModelFormHasDefaultModel = False
        , bActionModelFormTitleMsg = Just "MsgTestmailSendTestMail"
        , bActionModelFormRouteHsType = "AdminR"
        , bActionModelFields =
            [ BActionField
              { bActionFieldName = "email"
              , bActionFieldLabelDe = Just "Email"
              , bActionFieldLabelEn = Just "Email"
              , bActionFieldHsType = "Text"
              , bActionFieldFormFieldType = Just "textField"
              , bActionFieldView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bActionModelTranslations =
          Just
          [ BTranslation { bTranslationKey = "testMail", bTranslationDe = "Test-Mail", bTranslationEn = "Test-Mail" }
          , BTranslation { bTranslationKey = "sendTestMail", bTranslationDe = "Test-Mail senden...", bTranslationEn = "Send Test-Mail..." }
          ]
        }

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
    , BTranslation { bTranslationKey = "info", bTranslationDe = "Info", bTranslationEn = "Info" }
    ]
  }
