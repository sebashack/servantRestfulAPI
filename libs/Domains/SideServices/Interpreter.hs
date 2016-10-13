{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domains.SideServices.Interpreter ( sideServer ) where

import Data.ByteString.Char8 (pack)
import Control.Monad.Except
import Network.Wai
import Servant 
import Data.Aeson
import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Domains.SideServices.API
import Configs.ConfigTypes
import HelperLibs.MySQL.ActionRunner
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Servant.JuicyPixels
import HelperLibs.Interpreters.BookingDomain
import HelperLibs.Interpreters.SideServices
import Data.Maybe
import Data.Monoid
import Crypto.PasswordStore
import qualified Web.JWT as JWT
import qualified Data.ByteString.Base64 as B64
import qualified Database.Persist.MySQL as MySQL
import qualified Domains.SideServices.DataTypes as SST
import qualified Schemas.SQL.User as USCH
import qualified Schemas.SQL.BookableImage as BKSCH
import qualified Schemas.SQL.PropertyImage as PKSCH
import qualified Schemas.SQL.DbTypes as DbT
import qualified Repositories.SideServicesRepo.Operations as SSR
import qualified Repositories.UserRepo.Operations as UR (getUserPassByProfNameOrMainEmail,
                                                         updatePassword,
                                                         getUserRecCode,
                                                         invalidateUserSecret)
import qualified Data.ByteString.Lazy as LB (toStrict, fromStrict)
import qualified Data.ByteString as SB (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
-- HTML Rendering
import Text.Blaze.Internal (textValue)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html.Renderer.Text as HT
import qualified Text.Blaze.Html5.Attributes as HA
-- SMPT and SSL 
import qualified Network.HaskellNet.SMTP as SMTP
import Network.HaskellNet.SMTP.SSL



{-
  Given a bookable's imageId return that image in JPEG.
  The image can be scaled into the following sizes:
    mini: 100x100 
    avatar: 200x200 
    bigAvatar: 250x250 
    medium: 530x250 
    standard: 840x460 
    large: 920x540 
-}
getBookableImage :: ConfigMySQL
                 -> T.Text
                 -> Maybe T.Text
                 -> ExceptT ServantErr IO DynamicImage
getBookableImage config imgId imgSize = do
  bklImage <- liftIO $ runMySQL config $ do
    eitherImg <- SSR.getBookableImage imgId
    return $ returnDecodedImg eitherImg imgSize 
  either (\err -> throwError err) (\v -> return v) (getImgCustomError bklImage)          



{-
  Given a property's imageId return that image in JPEG.
  The image can be scaled into the following sizes:
    mini: 100x100 
    avatar: 200x200 
    bigAvatar: 250x250 
    medium: 530x250 
    standard: 840x460 
    large: 920x540
-}
getPropertyImage :: ConfigMySQL
                 -> T.Text
                 -> Maybe T.Text
                 -> ExceptT ServantErr IO DynamicImage
getPropertyImage config imgId imgSize = do
  propImage <- liftIO $ runMySQL config $ do
    eitherImg <- SSR.getPropertyImage imgId
    return $ returnDecodedImg eitherImg imgSize 
  either (\err -> throwError err) (\v -> return v) (getImgCustomError propImage)          



{-
  Given a user's avatarId return that image in JPEG.
  The image can be scaled into the following sizes:
    mini: 100x100 
    avatar: 200x200 
    bigAvatar: 250x250 
    medium: 530x250 
    standard: 840x460 
    large: 920x540

-}
getUserAvatar :: ConfigMySQL
                 -> T.Text
                 -> Maybe T.Text
                 -> ExceptT ServantErr IO DynamicImage
getUserAvatar config imgId imgSize = do
  avatar <- liftIO $ runMySQL config $ do
    eitherImg <- SSR.getUserAvatar imgId
    return $ returnDecodedImg eitherImg imgSize 
  either (\err -> throwError err) (\v -> return v) (getImgCustomError avatar)          




emailRecCode :: ConfigMySQL
             -> Maybe T.Text
             -> T.Text
             -> ExceptT ServantErr IO ()
emailRecCode config lang identifier = do
  update <- liftIO $ runMySQL config $ do
    userCreds <- UR.getUserPassByProfNameOrMainEmail identifier
    case userCreds of
      Nothing -> return userNotFound
      Just (userId, profName, _, email) -> do
        recCode <- SSR.updateRecoveryCode userId
        let from = "forgotPassword@destinos.io"
            to = T.unpack email
            subject = "Forgot Password"
            body = ""
            html = HT.renderHtml $ genHtml profName (TE.decodeUtf8 recCode) isSpanish  
            host = "email-smtp.us-west-2.amazonaws.com" :: String
            userName = "AKIAIPIC5QDL5DEVD3CQ" :: String
            pass = "Avqytd0xYW4BnXo0KQ7cpRD4YXqdCqu/MWi1yRr7A38P" :: String            
        liftIO $ doSMTPSTARTTLSWithSettings host defaultSettingsSMTPSTARTTLS $ \c -> do
          authSucceed <- SMTP.authenticate SMTP.LOGIN userName pass c
          if authSucceed
          then do
            sendMimeMail to from subject body html [] c
            return $ Right ()
          else do
            print "Error While Authenticating Email Service"
            return emailErr
  either (\err -> throwError err) (\v -> return v) (getCustomError update)
  where
    isSpanish = maybe False (\v -> v == "esp" || v == "es") lang
    genHtml profName code esp = do
      let aText = "destinos.io/resetPassword" :: T.Text
          hrefText = "destinos.io/" <> (textValue profName) <> "/" <> (textValue code)
      H5.h3 $ H.toHtml ("destinosIO" :: T.Text)
      H5.p $ H.toHtml p1Text
      H5.p $ H.toHtml p2Text
      (H5.a $ H.toHtml aText) H5.! HA.href hrefText H5.! HA.target "_blank"
      H5.p $ H.toHtml p3Text
      H5.p $ H.toHtml p4Text
      where
        p1Text = if esp
                 then "Hemos recibido una solicitud para renovar la contraseña asociada a esta dirección de email."
                 else "We received a request to reset the password associated with this email address." :: T.Text
        p2Text = if esp
                 then "Si usted hizo esta solicitud, porfavor haga click en el link de abajo para renovar su contraseña:"
                 else "If you made this request, please click the link below to reset your password: " :: T.Text
        p3Text = if esp
                 then "Si usted no solicitó renovar su contraseña usted puede ignorar este email de forma segura."
                 else"If you did not request to have your password reset you can safely ignore this email." :: T.Text
        p4Text = if esp
                 then "El link de arriba solamente es válido por 15 minutos después de haberse hecho la solicitud."
                 else "The link above is only valid for 15 minutes after you made this request." :: T.Text




resetPassword :: ConfigMySQL
              -> SST.ResetPassData
              -> ExceptT ServantErr IO ()
resetPassword config (SST.ResetPassData profName recCode newPass) = do
  update <- liftIO $ runMySQL config $ do
    recCredentials <- UR.getUserRecCode profName
    case recCredentials of
      (Nothing, Nothing, Nothing) -> return userNotFound
      (_, Nothing, _) -> return noRecCode
      (_, _, Nothing) -> return noRecCode
      (Just userId, Just hash, Just exp) -> do
        currentDate <- liftIO TM.getCurrentTime
        let isExpired = exp >= currentDate  
            matching = verifyPassword (TE.encodeUtf8 recCode) hash
        case (isExpired, matching, validatePassword newPass) of
          (False, _, _) -> return expiredRecCode
          (_, False, _) -> return recCodeMismatch
          (_, _, Left error) -> return $ Left error
          (True, True, Right _) -> do
            UR.updatePassword userId newPass
            SSR.invalidateUserRecCode userId
            UR.invalidateUserSecret userId
            return $ Right ()
  either (\err -> throwError err) (\v -> return v) (getCustomError update)



sideServer :: ConfigMySQL -> Server SideAPI
sideServer config = (getBookableImage config)
               :<|> (getPropertyImage config)
               :<|> (getUserAvatar config)
               :<|> (emailRecCode config)
               :<|> (resetPassword config)
