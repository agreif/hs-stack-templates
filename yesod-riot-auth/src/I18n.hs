{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module I18n where

import ClassyPrelude.Yesod

-- gen i18n - start
data AppMessage
  = MsgGlobalHome
  | MsgGlobalAdmin
  | MsgGlobalLogout
  | MsgGlobalLanguage
  | MsgGlobalMyProfile
  | MsgGlobalEditMyProfile
  | MsgGlobalReallyDelete
  | MsgGlobalCancel
  | MsgGlobalInfo
  | MsgUserIdent
  | MsgUserPassword
  | MsgUserEmail
  | MsgUserIsAdmin
  | MsgUserIsResetPassword
  | MsgConfigCode
  | MsgConfigStringValue
  | MsgConfigIntValue
  | MsgConfigDoubleValue
  | MsgConfigBoolValue
  | MsgTestmailEmail
  | MsgDemoaMyattr
  | MsgDemobMyattr
  | MsgDemocDemobId
  | MsgDemocMyattr
  | MsgUserUsers
  | MsgUserAddUser
  | MsgUserEditUser
  | MsgUserDeleteUser
  | MsgConfigConfigurations
  | MsgConfigEditConfig
  | MsgTestmailTestMail
  | MsgTestmailSendTestMail
  | MsgDemoaDemoa
  | MsgDemoaDemoas
  | MsgDemoaAddDemoa
  | MsgDemoaEditDemoa
  | MsgDemoaDeleteDemoa
  | MsgDemobDemob
  | MsgDemobDemobs
  | MsgDemobAddDemob
  | MsgDemobEditDemob
  | MsgDemobDeleteDemob
  | MsgDemocDemoc
  | MsgDemocDemocs
  | MsgDemocAddDemoc
  | MsgDemocEditDemoc
  | MsgDemocDeleteDemoc

renderMessageGerman :: AppMessage -> Text
renderMessageGerman MsgGlobalHome = "Home"
renderMessageGerman MsgGlobalAdmin = "Admin"
renderMessageGerman MsgGlobalLogout = "Logout"
renderMessageGerman MsgGlobalLanguage = "Sprache"
renderMessageGerman MsgGlobalMyProfile = "Mein Profil"
renderMessageGerman MsgGlobalEditMyProfile = "Mein Profil bearbeiten"
renderMessageGerman MsgGlobalReallyDelete = "Möchten sie wirklich löschen?"
renderMessageGerman MsgGlobalCancel = "Abbrechen"
renderMessageGerman MsgGlobalInfo = "Info"
renderMessageGerman MsgUserIdent = "Login"
renderMessageGerman MsgUserPassword = "Passwort"
renderMessageGerman MsgUserEmail = "Email"
renderMessageGerman MsgUserIsAdmin = "Ist Admin?"
renderMessageGerman MsgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
renderMessageGerman MsgConfigCode = "Code"
renderMessageGerman MsgConfigStringValue = "String-Wert"
renderMessageGerman MsgConfigIntValue = "Integer-Wert"
renderMessageGerman MsgConfigDoubleValue = "Double-Wert"
renderMessageGerman MsgConfigBoolValue = "Boolean-Wert"
renderMessageGerman MsgTestmailEmail = "Email"
renderMessageGerman MsgDemoaMyattr = "My attribute"
renderMessageGerman MsgDemobMyattr = "My attribute"
renderMessageGerman MsgDemocDemobId = ""
renderMessageGerman MsgDemocMyattr = "My attribute"
renderMessageGerman MsgUserUsers = "Nutzer"
renderMessageGerman MsgUserAddUser = "Nutzer hinzufügen"
renderMessageGerman MsgUserEditUser = "Nutzer bearbeiten"
renderMessageGerman MsgUserDeleteUser = "Nutzer löschen"
renderMessageGerman MsgConfigConfigurations = "Konfigurationen"
renderMessageGerman MsgConfigEditConfig = "Konfiguration bearbeiten"
renderMessageGerman MsgTestmailTestMail = "Test-Mail"
renderMessageGerman MsgTestmailSendTestMail = "Test-Mail senden..."
renderMessageGerman MsgDemoaDemoa = "Demoa"
renderMessageGerman MsgDemoaDemoas = "Demoas"
renderMessageGerman MsgDemoaAddDemoa = "Demoa hinzufügen"
renderMessageGerman MsgDemoaEditDemoa = "Demoa bearbeiten"
renderMessageGerman MsgDemoaDeleteDemoa = "Demoa löschen"
renderMessageGerman MsgDemobDemob = "Demob"
renderMessageGerman MsgDemobDemobs = "Demobs"
renderMessageGerman MsgDemobAddDemob = "Demob hinzufügen"
renderMessageGerman MsgDemobEditDemob = "Demob bearbeiten"
renderMessageGerman MsgDemobDeleteDemob = "Demob löschen"
renderMessageGerman MsgDemocDemoc = "Democ"
renderMessageGerman MsgDemocDemocs = "Democs"
renderMessageGerman MsgDemocAddDemoc = "Democ hinzufügen"
renderMessageGerman MsgDemocEditDemoc = "Democ bearbeiten"
renderMessageGerman MsgDemocDeleteDemoc = "Democ löschen"

renderMessageEnglish :: AppMessage -> Text
renderMessageEnglish MsgGlobalHome = "Home"
renderMessageEnglish MsgGlobalAdmin = "Admin"
renderMessageEnglish MsgGlobalLogout = "Logout"
renderMessageEnglish MsgGlobalLanguage = "Language"
renderMessageEnglish MsgGlobalMyProfile = "My Profile"
renderMessageEnglish MsgGlobalEditMyProfile = "Edit my profile"
renderMessageEnglish MsgGlobalReallyDelete = "Are you sure to delete?"
renderMessageEnglish MsgGlobalCancel = "Cancel"
renderMessageEnglish MsgGlobalInfo = "Info"
renderMessageEnglish MsgUserIdent = "Login"
renderMessageEnglish MsgUserPassword = "Password"
renderMessageEnglish MsgUserEmail = "Email"
renderMessageEnglish MsgUserIsAdmin = "Is admin?"
renderMessageEnglish MsgUserIsResetPassword = "Generate new password? (Will be sent by email)"
renderMessageEnglish MsgConfigCode = "Code"
renderMessageEnglish MsgConfigStringValue = "String-Value"
renderMessageEnglish MsgConfigIntValue = "Integer-Value"
renderMessageEnglish MsgConfigDoubleValue = "Double-Value"
renderMessageEnglish MsgConfigBoolValue = "Boolean-Value"
renderMessageEnglish MsgTestmailEmail = "Email"
renderMessageEnglish MsgDemoaMyattr = "Mein Attribut"
renderMessageEnglish MsgDemobMyattr = "Mein Attribut"
renderMessageEnglish MsgDemocDemobId = ""
renderMessageEnglish MsgDemocMyattr = "Mein Attribut"
renderMessageEnglish MsgUserUsers = "Users"
renderMessageEnglish MsgUserAddUser = "Add user"
renderMessageEnglish MsgUserEditUser = "Edit user"
renderMessageEnglish MsgUserDeleteUser = "Delete user"
renderMessageEnglish MsgConfigConfigurations = "Configurations"
renderMessageEnglish MsgConfigEditConfig = "Edit config"
renderMessageEnglish MsgTestmailTestMail = "Test-Mail"
renderMessageEnglish MsgTestmailSendTestMail = "Send Test-Mail..."
renderMessageEnglish MsgDemoaDemoa = "Demoa"
renderMessageEnglish MsgDemoaDemoas = "Demoas"
renderMessageEnglish MsgDemoaAddDemoa = "Add demoa"
renderMessageEnglish MsgDemoaEditDemoa = "Edit demoa"
renderMessageEnglish MsgDemoaDeleteDemoa = "Delete demoa"
renderMessageEnglish MsgDemobDemob = "Demob"
renderMessageEnglish MsgDemobDemobs = "Demobs"
renderMessageEnglish MsgDemobAddDemob = "Add demob"
renderMessageEnglish MsgDemobEditDemob = "Edit demob"
renderMessageEnglish MsgDemobDeleteDemob = "Delete demob"
renderMessageEnglish MsgDemocDemoc = "Democ"
renderMessageEnglish MsgDemocDemocs = "Democs"
renderMessageEnglish MsgDemocAddDemoc = "Add democ"
renderMessageEnglish MsgDemocEditDemoc = "Edit democ"
renderMessageEnglish MsgDemocDeleteDemoc = "Delete democ"

data Translation = Translation
  { msgGlobalHome :: Text,
    msgGlobalAdmin :: Text,
    msgGlobalLogout :: Text,
    msgGlobalLanguage :: Text,
    msgGlobalMyProfile :: Text,
    msgGlobalEditMyProfile :: Text,
    msgGlobalReallyDelete :: Text,
    msgGlobalCancel :: Text,
    msgGlobalInfo :: Text,
    msgUserIdent :: Text,
    msgUserPassword :: Text,
    msgUserEmail :: Text,
    msgUserIsAdmin :: Text,
    msgUserIsResetPassword :: Text,
    msgConfigCode :: Text,
    msgConfigStringValue :: Text,
    msgConfigIntValue :: Text,
    msgConfigDoubleValue :: Text,
    msgConfigBoolValue :: Text,
    msgTestmailEmail :: Text,
    msgDemoaMyattr :: Text,
    msgDemobMyattr :: Text,
    msgDemocDemobId :: Text,
    msgDemocMyattr :: Text,
    msgUserUsers :: Text,
    msgUserAddUser :: Text,
    msgUserEditUser :: Text,
    msgUserDeleteUser :: Text,
    msgConfigConfigurations :: Text,
    msgConfigEditConfig :: Text,
    msgTestmailTestMail :: Text,
    msgTestmailSendTestMail :: Text,
    msgDemoaDemoa :: Text,
    msgDemoaDemoas :: Text,
    msgDemoaAddDemoa :: Text,
    msgDemoaEditDemoa :: Text,
    msgDemoaDeleteDemoa :: Text,
    msgDemobDemob :: Text,
    msgDemobDemobs :: Text,
    msgDemobAddDemob :: Text,
    msgDemobEditDemob :: Text,
    msgDemobDeleteDemob :: Text,
    msgDemocDemoc :: Text,
    msgDemocDemocs :: Text,
    msgDemocAddDemoc :: Text,
    msgDemocEditDemoc :: Text,
    msgDemocDeleteDemoc :: Text
  }
  deriving (Generic)

instance ToJSON Translation

translationDe :: Translation
translationDe =
  Translation
    { msgGlobalHome = "Home",
      msgGlobalAdmin = "Admin",
      msgGlobalLogout = "Logout",
      msgGlobalLanguage = "Sprache",
      msgGlobalMyProfile = "Mein Profil",
      msgGlobalEditMyProfile = "Mein Profil bearbeiten",
      msgGlobalReallyDelete = "Möchten sie wirklich löschen?",
      msgGlobalCancel = "Abbrechen",
      msgGlobalInfo = "Info",
      msgUserIdent = "Login",
      msgUserPassword = "Passwort",
      msgUserEmail = "Email",
      msgUserIsAdmin = "Ist Admin?",
      msgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)",
      msgConfigCode = "Code",
      msgConfigStringValue = "String-Wert",
      msgConfigIntValue = "Integer-Wert",
      msgConfigDoubleValue = "Double-Wert",
      msgConfigBoolValue = "Boolean-Wert",
      msgTestmailEmail = "Email",
      msgDemoaMyattr = "My attribute",
      msgDemobMyattr = "My attribute",
      msgDemocDemobId = "",
      msgDemocMyattr = "My attribute",
      msgUserUsers = "Nutzer",
      msgUserAddUser = "Nutzer hinzufügen",
      msgUserEditUser = "Nutzer bearbeiten",
      msgUserDeleteUser = "Nutzer löschen",
      msgConfigConfigurations = "Konfigurationen",
      msgConfigEditConfig = "Konfiguration bearbeiten",
      msgTestmailTestMail = "Test-Mail",
      msgTestmailSendTestMail = "Test-Mail senden...",
      msgDemoaDemoa = "Demoa",
      msgDemoaDemoas = "Demoas",
      msgDemoaAddDemoa = "Demoa hinzufügen",
      msgDemoaEditDemoa = "Demoa bearbeiten",
      msgDemoaDeleteDemoa = "Demoa löschen",
      msgDemobDemob = "Demob",
      msgDemobDemobs = "Demobs",
      msgDemobAddDemob = "Demob hinzufügen",
      msgDemobEditDemob = "Demob bearbeiten",
      msgDemobDeleteDemob = "Demob löschen",
      msgDemocDemoc = "Democ",
      msgDemocDemocs = "Democs",
      msgDemocAddDemoc = "Democ hinzufügen",
      msgDemocEditDemoc = "Democ bearbeiten",
      msgDemocDeleteDemoc = "Democ löschen"
    }

translationEn :: Translation
translationEn =
  Translation
    { msgGlobalHome = "Home",
      msgGlobalAdmin = "Admin",
      msgGlobalLogout = "Logout",
      msgGlobalLanguage = "Language",
      msgGlobalMyProfile = "My Profile",
      msgGlobalEditMyProfile = "Edit my profile",
      msgGlobalReallyDelete = "Are you sure to delete?",
      msgGlobalCancel = "Cancel",
      msgGlobalInfo = "Info",
      msgUserIdent = "Login",
      msgUserPassword = "Password",
      msgUserEmail = "Email",
      msgUserIsAdmin = "Is admin?",
      msgUserIsResetPassword = "Generate new password? (Will be sent by email)",
      msgConfigCode = "Code",
      msgConfigStringValue = "String-Value",
      msgConfigIntValue = "Integer-Value",
      msgConfigDoubleValue = "Double-Value",
      msgConfigBoolValue = "Boolean-Value",
      msgTestmailEmail = "Email",
      msgDemoaMyattr = "Mein Attribut",
      msgDemobMyattr = "Mein Attribut",
      msgDemocDemobId = "",
      msgDemocMyattr = "Mein Attribut",
      msgUserUsers = "Users",
      msgUserAddUser = "Add user",
      msgUserEditUser = "Edit user",
      msgUserDeleteUser = "Delete user",
      msgConfigConfigurations = "Configurations",
      msgConfigEditConfig = "Edit config",
      msgTestmailTestMail = "Test-Mail",
      msgTestmailSendTestMail = "Send Test-Mail...",
      msgDemoaDemoa = "Demoa",
      msgDemoaDemoas = "Demoas",
      msgDemoaAddDemoa = "Add demoa",
      msgDemoaEditDemoa = "Edit demoa",
      msgDemoaDeleteDemoa = "Delete demoa",
      msgDemobDemob = "Demob",
      msgDemobDemobs = "Demobs",
      msgDemobAddDemob = "Add demob",
      msgDemobEditDemob = "Edit demob",
      msgDemobDeleteDemob = "Delete demob",
      msgDemocDemoc = "Democ",
      msgDemocDemocs = "Democs",
      msgDemocAddDemoc = "Add democ",
      msgDemocEditDemoc = "Edit democ",
      msgDemocDeleteDemoc = "Delete democ"
    }
-- gen i18n - end
