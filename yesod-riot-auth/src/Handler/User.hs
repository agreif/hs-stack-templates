{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User where

import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Handler.Mailer
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-------------------------------------------------------
-- add user
-------------------------------------------------------

-- gen data add - start
data VAddUser = VAddUser
  { vAddUserIdent :: Text,
    vAddUserEmail :: Text,
    vAddUserIsAdmin :: Bool
  }

-- gen data add - end

-- gen get add form - start
getAddUserFormR :: Handler Html
getAddUserFormR = do
  (formWidget, _) <- generateFormPost $ vAddUserForm Nothing Nothing
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgUserAddUser}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ AddUserR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get add form - end

postAddUserR :: Handler Value
postAddUserR = do
  ((result, formWidget), _) <- runFormPost $ vAddUserForm Nothing Nothing
  case result of
    FormSuccess vAddUser -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      (passwd, passwdHash) <- liftIO $ generatePassword 32
      urlRenderer <- getUrlRender
      let user =
            User
              { userIdent = vAddUserIdent vAddUser,
                userPassword = (Just passwdHash),
                userEmail = vAddUserEmail vAddUser,
                userIsAdmin = vAddUserIsAdmin vAddUser,
                userVersion = 1,
                userCreatedAt = curTime,
                userCreatedBy = userIdent authUser,
                userUpdatedAt = curTime,
                userUpdatedBy = userIdent authUser
              }
      _ <- runDB $ insert user
      sendPasswordNewAccountMail user passwd
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen add form - start
vAddUserForm :: Maybe UserId -> Maybe User -> Html -> MForm Handler (FormResult VAddUser, Widget)
vAddUserForm maybeUserId maybeUser extra = do
  (identResult, identView) <-
    mreq
      textField
      identFs
      (userIdent <$> maybeUser)
  (emailResult, emailView) <-
    mreq
      textField
      emailFs
      (userEmail <$> maybeUser)
  (isAdminResult, isAdminView) <-
    mreq
      checkBoxField
      isAdminFs
      (userIsAdmin <$> maybeUser)
  let vAddUserResult = VAddUser <$> identResult <*> emailResult <*> isAdminResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div #identInputWidget .uk-margin-small :not $ null $ fvErrors identView:.uk-form-danger>
      <label #identInputLabel .uk-form-label :not $ null $ fvErrors identView:.uk-text-danger for=#{fvId identView}>#{fvLabel identView}
      <div .uk-form-controls>
        ^{fvInput identView}
        <span #identInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgUserIdentInputInfo}
        $maybe err <- fvErrors identView
          <br>
          <span #identInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #emailInputWidget .uk-margin-small :not $ null $ fvErrors emailView:.uk-form-danger>
      <label #emailInputLabel .uk-form-label :not $ null $ fvErrors emailView:.uk-text-danger for=#{fvId emailView}>#{fvLabel emailView}
      <div .uk-form-controls>
        ^{fvInput emailView}
        <span #emailInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgUserEmailInputInfo}
        $maybe err <- fvErrors emailView
          <br>
          <span #emailInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #isAdminInputWidget .uk-margin-small :not $ null $ fvErrors isAdminView:.uk-form-danger>
      <label #isAdminInputLabel .uk-form-label :not $ null $ fvErrors isAdminView:.uk-text-danger for=#{fvId isAdminView}>#{fvLabel isAdminView}
      <div .uk-form-controls>
        ^{fvInput isAdminView}
        <span #isAdminInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgUserIsAdminInputInfo}
        $maybe err <- fvErrors isAdminView
          <br>
          <span #isAdminInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vAddUserResult, formWidget)
  where
    identFs :: FieldSettings App
    identFs =
      FieldSettings
        { fsLabel = SomeMessage MsgUserIdent,
          fsTooltip = Nothing,
          fsId = Just "ident",
          fsName = Just "ident",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    emailFs :: FieldSettings App
    emailFs =
      FieldSettings
        { fsLabel = SomeMessage MsgUserEmail,
          fsTooltip = Nothing,
          fsId = Just "email",
          fsName = Just "email",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    isAdminFs :: FieldSettings App
    isAdminFs =
      FieldSettings
        { fsLabel = SomeMessage MsgUserIsAdmin,
          fsTooltip = Nothing,
          fsId = Just "isAdmin",
          fsName = Just "isAdmin",
          fsAttrs = [("class", "uk-checkbox")]
        }

-- gen add form - end

-------------------------------------------------------
-- edit user
-------------------------------------------------------

-- gen data edit - start
data VEditUser = VEditUser
  { vEditUserIdent :: Text,
    vEditUserEmail :: Text,
    vEditUserIsAdmin :: Bool,
    vEditUserIsResetPassword :: Bool,
    vEditUserVersion :: Int
  }

-- gen data edit - end

-- gen get edit form - start
getEditUserFormR :: UserId -> Handler Html
getEditUserFormR userId = do
  user <- runDB $ get404 userId
  (formWidget, _) <- generateFormPost $ vEditUserForm (Just userId) (Just user)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgUserEditUser}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ EditUserR userId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get edit form - end

postEditUserR :: UserId -> Handler Value
postEditUserR userId = do
  ((result, formWidget), _) <- runFormPost $ vEditUserForm (Just userId) Nothing
  case result of
    FormSuccess vEditUser -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      (passwd, passwdHash) <- liftIO $ generatePassword 32
      urlRenderer <- getUrlRender
      let persistFields =
            [ UserIdent =. vEditUserIdent vEditUser,
              UserEmail =. vEditUserEmail vEditUser,
              UserIsAdmin =. vEditUserIsAdmin vEditUser,
              UserVersion =. vEditUserVersion vEditUser + 1,
              UserUpdatedAt =. curTime,
              UserUpdatedBy =. userIdent authUser
            ]
      let persistFields' =
            persistFields
              ++ if vEditUserIsResetPassword vEditUser
                then [UserPassword =. (Just passwdHash)]
                else []
      updateCount <-
        runDB $
          updateWhereCount
            [ UserId ==. userId,
              UserVersion ==. vEditUserVersion vEditUser
            ]
            persistFields'
      when (vEditUserIsResetPassword vEditUser) $ do
        user' <- runDB $ get404 userId
        sendPasswordResetMail user' passwd
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ AdminR AdminDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen edit form - start
vEditUserForm :: Maybe UserId -> Maybe User -> Html -> MForm Handler (FormResult VEditUser, Widget)
vEditUserForm maybeUserId maybeUser extra = do
  (identResult, identView) <-
    mreq
      textField
      identFs
      (userIdent <$> maybeUser)
  (emailResult, emailView) <-
    mreq
      textField
      emailFs
      (userEmail <$> maybeUser)
  (isAdminResult, isAdminView) <-
    mreq
      checkBoxField
      isAdminFs
      (userIsAdmin <$> maybeUser)
  (isResetPasswordResult, isResetPasswordView) <-
    mreq
      checkBoxField
      isResetPasswordFs
      (Nothing)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (userVersion <$> maybeUser)
  let vEditUserResult = VEditUser <$> identResult <*> emailResult <*> isAdminResult <*> isResetPasswordResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div #identInputWidget .uk-margin-small :not $ null $ fvErrors identView:.uk-form-danger>
      <label #identInputLabel .uk-form-label :not $ null $ fvErrors identView:.uk-text-danger for=#{fvId identView}>#{fvLabel identView}
      <div .uk-form-controls>
        ^{fvInput identView}
        <span #identInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgUserIdentInputInfo}
        $maybe err <- fvErrors identView
          <br>
          <span #identInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #emailInputWidget .uk-margin-small :not $ null $ fvErrors emailView:.uk-form-danger>
      <label #emailInputLabel .uk-form-label :not $ null $ fvErrors emailView:.uk-text-danger for=#{fvId emailView}>#{fvLabel emailView}
      <div .uk-form-controls>
        ^{fvInput emailView}
        <span #emailInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgUserEmailInputInfo}
        $maybe err <- fvErrors emailView
          <br>
          <span #emailInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #isAdminInputWidget .uk-margin-small :not $ null $ fvErrors isAdminView:.uk-form-danger>
      <label #isAdminInputLabel .uk-form-label :not $ null $ fvErrors isAdminView:.uk-text-danger for=#{fvId isAdminView}>#{fvLabel isAdminView}
      <div .uk-form-controls>
        ^{fvInput isAdminView}
        <span #isAdminInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgUserIsAdminInputInfo}
        $maybe err <- fvErrors isAdminView
          <br>
          <span #isAdminInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #isResetPasswordInputWidget .uk-margin-small :not $ null $ fvErrors isResetPasswordView:.uk-form-danger>
      <label #isResetPasswordInputLabel .uk-form-label :not $ null $ fvErrors isResetPasswordView:.uk-text-danger for=#{fvId isResetPasswordView}>#{fvLabel isResetPasswordView}
      <div .uk-form-controls>
        ^{fvInput isResetPasswordView}
        <span #isResetPasswordInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgUserIsResetPasswordInputInfo}
        $maybe err <- fvErrors isResetPasswordView
          <br>
          <span #isResetPasswordInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vEditUserResult, formWidget)
  where
    identFs :: FieldSettings App
    identFs =
      FieldSettings
        { fsLabel = SomeMessage MsgUserIdent,
          fsTooltip = Nothing,
          fsId = Just "ident",
          fsName = Just "ident",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    emailFs :: FieldSettings App
    emailFs =
      FieldSettings
        { fsLabel = SomeMessage MsgUserEmail,
          fsTooltip = Nothing,
          fsId = Just "email",
          fsName = Just "email",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    isAdminFs :: FieldSettings App
    isAdminFs =
      FieldSettings
        { fsLabel = SomeMessage MsgUserIsAdmin,
          fsTooltip = Nothing,
          fsId = Just "isAdmin",
          fsName = Just "isAdmin",
          fsAttrs = [("class", "uk-checkbox")]
        }
    isResetPasswordFs :: FieldSettings App
    isResetPasswordFs =
      FieldSettings
        { fsLabel = SomeMessage MsgUserIsResetPassword,
          fsTooltip = Nothing,
          fsId = Just "isResetPassword",
          fsName = Just "isResetPassword",
          fsAttrs = [("class", "uk-checkbox")]
        }
    versionFs :: FieldSettings App
    versionFs =
      FieldSettings
        { fsLabel = "",
          fsTooltip = Nothing,
          fsId = Just "version",
          fsName = Just "version",
          fsAttrs = []
        }

-- gen edit form - end

-------------------------------------------------------
-- delete user
-------------------------------------------------------

-- gen delete form - start
vDeleteUserForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteUserForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)

-- gen delete form - end

-- gen post delete - start
postDeleteUserR :: UserId -> Handler Value
postDeleteUserR userId = do
  curTime <- liftIO getCurrentTime
  Entity _ authUser <- requireAuth
  runDB $ do
    -- trick to record the user deleting the entity
    updateWhere
      [UserId ==. userId]
      [ UserUpdatedAt =. curTime,
        UserUpdatedBy =. userIdent authUser
      ]
    delete userId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR}

-- gen post delete - end

-- gen get delete form - start
getDeleteUserFormR :: UserId -> Handler Html
getDeleteUserFormR userId = do
  (formWidget, _) <- generateFormPost $ vDeleteUserForm
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgUserDeleteUser}
      <form #modal-form .uk-form-horizontal method=post action=@{AdminR $ DeleteUserR userId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end
