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

module Domains.UserDomain.User.Interpreter
  (
    userServer,
    basicAuthServerContext
  ) where

import Data.ByteString.Char8 (pack)
import Control.Monad.Except
import Network.Wai
import Servant
import Servant.API.BasicAuth
import Servant.JuicyPixels
import Data.Aeson
import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Domains.UserDomain.User.API
import Configs.ConfigTypes
import HelperLibs.MySQL.ActionRunner
import HelperLibs.Interpreters.User
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Crypto.PasswordStore
import HelperLibs.Interpreters.BookingDomain
import Data.Maybe
import qualified Web.JWT as JWT
import qualified Data.ByteString.Base64 as B64
import qualified Database.Persist.MySQL as MySQL
import qualified Data.CountryCodes as CC
import qualified Domains.UserDomain.User.DataTypes as UT
import qualified Schemas.SQL.User as USCH
import qualified Schemas.SQL.DbTypes as DbT
import qualified Repositories.UserRepo.Operations as UR
import qualified Data.ByteString.Lazy as LB (toStrict, fromStrict)
import qualified Data.ByteString as SB (length, ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE


type UserToken = T.Text

          
{-
Given all the necessary data to create a User, this endpoint creates a new User in the data base. 
The profName and mainEmail of the user are case sensitive.  
This function returns a NewUser which is a User together with an authentiction token.
Note that both profName and mainEmail are unique identifiers for a User.
Also note that second Email and phone number are optional params, so in case they are invalid, they 
won't be stored in DB.
-}
postUser :: ConfigMySQL
         -> UT.CreateUserData
         -> ExceptT ServantErr IO UT.NewUser
postUser config userData = do
  eitherNewUser <- liftIO $ runMySQL config $ do                
    areValidAuthFields <- validateCreateUserData userData 
    case areValidAuthFields of
      Left error -> return $ Left error 
      Right validatedData -> do
        let password = UT.password validatedData
            (UT.BasicUserData profName fName lName bDate cCode gen mEmail sEmail phNum) = UT.basicData validatedData
            country = CC.toText cCode
            repoGen = toRepoGender gen
            repoData = (profName, fName, lName, bDate, country, repoGen, mEmail, sEmail, phNum)
        (userId, secret, repoUser) <- UR.storeUser repoData password  
        let creationDate = USCH.userCreated repoUser
            user = UT.User (UT.basicData validatedData) creationDate False 0 Nothing
        (userToken, _) <- liftIO $ createUserToken userId profName secret 
        return $ Right (UT.NewUser user userToken)
  either (\err -> throwError err) (\newUser -> return newUser) (getCustomError eitherNewUser)      
  



-- Given a ProfName, this endpoint returns all the public data that is related to the user identified with that
-- profile name.
getUser :: ConfigMySQL
        -> Maybe T.Text
        -> ExceptT ServantErr IO UT.User
getUser _ Nothing = throwError _noToken 
getUser config (Just token) = do
  user <- liftIO $ runMySQL config $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, profName, _, _, _) -> do 
        eitherUser <-  UR.getUserById userId 
        case eitherUser of
          Left error -> return userNotFound 
          Right (userId, avatarId, repoUser) -> do
            let basicData = UT.BasicUserData profName
                                             (USCH.userName repoUser) 
                                             (USCH.userLastName repoUser)
                                             (USCH.userBirthDate repoUser)
                                             (CC.fromText $ USCH.userCountry repoUser)
                                             (toDomainGender $ USCH.userGender repoUser)                                   
                                             (TE.decodeUtf8 $ USCH.userMainEmail repoUser)
                                             (USCH.userSecondEmail repoUser)
                                             (USCH.userPhoneNum repoUser)                 
            return $ Right $ UT.User basicData 
                                     (USCH.userCreated repoUser) 
                                     (USCH.userBlockedAdmin repoUser) 
                                     (USCH.userCanCreate repoUser) 
                                     avatarId
  either (\err -> throwError err) (\v -> return v) (getCustomError user) 



{-
Given a BasicUserData data type, this function updates that User. Also note that this endpoint requires an 
authentication token.
This function returns a new token because the profile name is updated.
If the new profName provided is not valid, the old one will be preserved.
If the new mainEmail provided is not valid, the old one will be preserved.
Note that second Email and phone number are optional params, so in case they are invalid, a null will be stored
in DB.
-}
updateBasicData :: ConfigMySQL
                -> Maybe T.Text
                -> UT.BasicUserData
                -> ExceptT ServantErr IO UT.TokenInfo
updateBasicData config Nothing basicData = throwError _noToken
updateBasicData config (Just token) basicData = do
  update <- liftIO $ runMySQL config $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, oldProfName, oldMainEmail, oldHash, secret) -> do
        eitherBasicData <- validateBasicUserData basicData (Just oldProfName) (Just oldMainEmail)
        case eitherBasicData of
          Left error -> return $ Left error
          Right validatedData -> do
            let (UT.BasicUserData profName fName lName bDate cCode gen mEmail sEmail phNum) = validatedData
            secret <- UR.updateUserSecret userId
            (userToken, exp) <- liftIO $ createUserToken userId profName secret
            UR.updateUserData userId
                              (profName,
                               fName,
                               lName,
                               bDate,
                               (CC.toText cCode),
                               (toRepoGender gen),
                               mEmail,
                               sEmail,
                               phNum)
            return $ Right (userToken, exp)
  either (\err -> throwError err) (\(tkn, exp) -> return $ UT.TokenInfo tkn exp) (getCustomError update)      
    




-- Given a valid profName check if it is available.
isAvailableProfName :: ConfigMySQL
                    -> T.Text
                    -> ExceptT ServantErr IO Bool
isAvailableProfName _ profName
  | validateProfName profName == badProfName = return False
isAvailableProfName config profName = do
  isAvailable <- liftIO $ runMySQL config $ UR.profileNameAvailability profName
  return isAvailable




-- Given a valid email, check if it is available.
isAvailableMainEmail :: ConfigMySQL
                     -> T.Text
                     -> ExceptT ServantErr IO Bool
isAvailableMainEmail _ email
  | validateEmail email == badEmail = return False
isAvailableMainEmail config email = do
  isAvailable <- liftIO $ runMySQL config $ UR.emailAvailability email
  return isAvailable





-- Given a changePassData data type, this endpoint updates the password of a user.
-- Note that this endpoint requires an authentication token.
changeUserPassword :: ConfigMySQL
                   -> Maybe T.Text
                   -> UT.ChangePassData
                   -> ExceptT ServantErr IO ()
changeUserPassword _ _ passData
  | validatePassword (UT.newPassword passData) == badPass = throwError _badPass
changeUserPassword _ Nothing _ = throwError _noToken
changeUserPassword config (Just token) passData = do
  update <- liftIO $ runMySQL config $ do
    credentials <- validateUser token 
    case credentials of
      Left error -> return $ Left error
      Right (userId, _, _, oldHash, secret) -> 
        case verifyPassword (TE.encodeUtf8 oldPass) oldHash of
          False -> return passMismatch 
          True -> do            
            UR.updatePassword userId newPass
            return $ Right ()
  either (\err -> throwError err) (\v -> return v) (getCustomError update)      
  where
    (UT.ChangePassData oldPass newPass) = passData 
        



-- Given a jpeg image upload a User's avatar
uploadUserAvatar :: ConfigMySQL
                 -> Maybe T.Text
                 -> DynamicImage
                 -> ExceptT ServantErr IO T.Text
uploadUserAvatar _ Nothing _ = throwError _noToken
uploadUserAvatar config (Just token) (ImageYCbCr8 imgJPEG) = do
  eitherAvatar <- liftIO $ runMySQL config $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, _, _, hashedPass, secret) -> do            
        let encodedImg = LB.toStrict (encodeJpegAtQuality 100 imgJPEG)
            numBytes =  SB.length encodedImg
        case checkImgBound numBytes of
          LT -> return tooSmallImage 
          GT -> return tooLargeImage
          EQ -> do
            avatarId <- UR.storeUserAvatar userId encodedImg
            return $ Right avatarId
  either (\err -> throwError err) (\v -> return v) (getCustomError eitherAvatar)          
 


-- Delete a User's avatar.
deleteUserAvatar :: ConfigMySQL
                 -> Maybe T.Text
                 -> ExceptT ServantErr IO ()
deleteUserAvatar _ Nothing = throwError _noToken   
deleteUserAvatar config (Just token) = do
  deletion <- liftIO $ runMySQL config $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, _, _, hashedPass, secret) -> do
        UR.deleteUserAvatar userId
        return $ Right ()
  either (\err -> throwError err) (\v -> return v) (getCustomError deletion)      
  



-- Basic Authentication --

-- Basic Authentication returns a UserToken.  
authCheck :: ConfigMySQL -> BasicAuthCheck (UserToken, TM.UTCTime)
authCheck config = BasicAuthCheck check 
  where
    check (BasicAuthData identifier password) = do   
      credentials  <- runMySQL config $ do
        idAndPass <- UR.getUserPassByProfNameOrMainEmail (TE.decodeUtf8 identifier)
        case idAndPass of
          Nothing -> return Nothing
          Just (userId, profName, hashedPass, _) ->
            case verifyPassword password hashedPass of
              False -> return Nothing
              True -> do
                secret <- UR.updateUserSecret userId 
                token <- liftIO $ createUserToken userId profName secret
                return $ Just token
      return $ maybe Unauthorized (\token -> Authorized token) credentials         
     


-- The context that will be made available to request handlers.
basicAuthServerContext :: ConfigMySQL -> Context (BasicAuthCheck (UserToken, TM.UTCTime) ': '[])
basicAuthServerContext config = (authCheck config) :. EmptyContext


getAuthToken :: ConfigMySQL
             -> (UserToken, TM.UTCTime)
             -> ExceptT ServantErr IO UT.TokenInfo
getAuthToken config (token, expiration) = return $ UT.TokenInfo token expiration 



-- End of Basic Authentication --
invalidateAuthToken :: ConfigMySQL
                    -> Maybe T.Text
                    -> ExceptT ServantErr IO ()
invalidateAuthToken _ Nothing = throwError _noToken
invalidateAuthToken config (Just token) = do
  update  <- liftIO $ runMySQL config $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, _, _, _, _)-> do
        UR.invalidateUserSecret userId
        return $ Right ()
  either (\err -> throwError err) (\v -> return v) (getCustomError update)      



-- Given a valid Authentication Token, this function returns a new authentication token with prolonged expiration.
refreshAuthToken :: ConfigMySQL
                 -> Maybe T.Text
                 -> ExceptT ServantErr IO UT.TokenInfo
refreshAuthToken _ Nothing = throwError _noToken
refreshAuthToken config (Just token) = do
  update <- liftIO $ runMySQL config $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, _, _, _, _) -> do
        case getTokenUserIdAndProfName token of
          Left error -> return $ Left error
          Right (_, profName, _) -> do
            secret <- UR.updateUserSecret userId
            token <- liftIO $ createUserToken userId profName secret
            return $ Right token
  either (\err -> throwError err) (\(tkn, exp) -> return $ UT.TokenInfo tkn exp) (getCustomError update)      
 


               
userServer :: ConfigMySQL -> Server UserAPI
userServer config = (getUser config)
               :<|> (postUser config)
               :<|> (updateBasicData config)
               :<|> (isAvailableProfName config)    
               :<|> (isAvailableMainEmail config)
               :<|> (changeUserPassword config)
               :<|> (uploadUserAvatar config)
               :<|> (deleteUserAvatar config)
               :<|> (getAuthToken config) 
               :<|> (invalidateAuthToken config)               
               :<|> (refreshAuthToken config)




