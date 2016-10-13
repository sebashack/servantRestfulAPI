{-# LANGUAGE OverloadedStrings #-}

module Repositories.UserRepo.Operations where


import Data.Time
import Crypto.PasswordStore
import HelperLibs.MySQL.UserRepo
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString) 
import qualified Schemas.SQL.DbTypes as DbT
import qualified Data.Time as TM
import qualified Data.ByteString.Char8 as C8
import qualified Schemas.SQL.User as SCH
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Database.Esqueleto  as DBEsq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Persist as Per
import qualified Data.ByteString.Base64 as B64



type UserData = (T.Text,
                 T.Text,
                 T.Text,
                 TM.UTCTime,
                 T.Text,
                 DbT.Gender,
                 T.Text,
                 Maybe
                 T.Text,
                 Maybe T.Text)

type Password = T.Text

{-
Given the minimum data to create a User, this function stores a new user
in the MySQL data base specified in the config, and returns a User.
In case that either the profileName or the mainEmail is not available - another user in db
has already taken on of them - this function throws an Exception.
Note that profile names are stored as ByteStrings so that we allow exact matches.
It is importat to know that the secret that is stored in DB is a Base64 encoding. However,
the secret returned by this function is the original one.
-}


storeUser :: MonadIO m => UserData
                       -> Password
                       -> ReaderT MySQL.SqlBackend m (Integer, T.Text, SCH.User)
storeUser userData password = do
  currentDate <- liftIO TM.getCurrentTime
  hashedPass <- liftIO $ makePassword (TE.encodeUtf8 password) 17
  let user = SCH.User currentDate
                      (TE.encodeUtf8 profName)
                      fName
                      lName
                      bDate
                      country
                      gen
                      (TE.encodeUtf8 mEmail)
                      maybeEmail
                      maybePhone
                      False
                      0
                      hashedPass
                      Nothing 
                      (Just $ TM.addUTCTime  691200 currentDate) -- token expires 8 days after creation
                      Nothing
                      Nothing
  userKey <- Per.insert user
  let userId = toInteger $ MySQL.unSqlBackendKey (SCH.unUserKey userKey)
  secret <- liftIO $ genSecret userId
  Per.update userKey [SCH.UserSecret Per.=. (Just $ B64.encode secret)] 
  return (userId, TE.decodeUtf8 secret, user)
  where
    ( profName, fName, lName, bDate, country,
      gen, mEmail, maybeEmail, maybePhone    ) = userData




-- Given a profile name, this function returns the user identified with it, if it exists.
-- The secret returned is the base64 encoded version.
-- Note that the avatarId of that user is also returned.
getUserByProfName :: MonadIO m => T.Text
                               -> ReaderT MySQL.SqlBackend m (Either String (Integer, Maybe T.Text, SCH.User))
getUserByProfName profName = do
  maybeUser <- Per.getBy $ SCH.UniqueProfileName (TE.encodeUtf8 profName)
  case maybeUser of
    Nothing -> return $ Left ("User with profile name '" ++ (T.unpack profName) ++ "' was not found")
    Just (Per.Entity userKey user) -> do
      let userId = toInteger $ MySQL.unSqlBackendKey (SCH.unUserKey userKey)   
      maybeAvatar <- (DBEsq.select $ DBEsq.from (\(profAvatar) -> do
                        (DBEsq.limit 1)                            
                        DBEsq.where_ (profAvatar DBEsq.^. SCH.ProfileAvatarUserId
                                      DBEsq.==. DBEsq.val userKey)
                        return $ profAvatar DBEsq.^. SCH.ProfileAvatarAvatarId))
      case maybeAvatar of
        [] -> return $ Right (userId, Nothing, user)
        [DBEsq.Value val] -> return $ Right (userId, Just $ TE.decodeUtf8 val, user)




-- Given a userId, this function returns the user identified with it, if it exists.
-- The secret returned is the base64 encoded version.
-- Note that the avatarId of that user is also returned.
getUserById :: MonadIO m => Integer
                         -> ReaderT MySQL.SqlBackend m (Either String (Integer, Maybe T.Text, SCH.User))
getUserById userId = do
  let userKey =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  maybeUser <- Per.get userKey
  case maybeUser of
    Nothing -> return $ Left ("User with id " ++ (show userId) ++ " was not found")
    Just user -> do   
      maybeAvatar <- (DBEsq.select $ DBEsq.from (\(profAvatar) -> do
                        (DBEsq.limit 1)                            
                        DBEsq.where_ (profAvatar DBEsq.^. SCH.ProfileAvatarUserId
                                      DBEsq.==. DBEsq.val userKey)
                        return $ profAvatar DBEsq.^. SCH.ProfileAvatarAvatarId))
      case maybeAvatar of
        [] -> return $ Right (userId, Nothing, user)
        [DBEsq.Value val] -> return $ Right (userId, Just $ TE.decodeUtf8 val, user)




-- Given a User's id return data necessary for authentication purposes: profName, mainEmail, passWord, secret, secretExp.
-- The secret returned by this function is the decoded Base64 one.
getUserAuthData :: MonadIO m => Integer
                             -> ReaderT MySQL.SqlBackend m (Maybe T.Text, Maybe T.Text, Maybe ByteString, Maybe T.Text, Maybe TM.UTCTime)
getUserAuthData userId = do
  let userKey =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  secret <- (DBEsq.select $ DBEsq.from (\(user) -> do
                (DBEsq.limit 1)                            
                DBEsq.where_ (user DBEsq.^. SCH.UserId
                              DBEsq.==. DBEsq.val userKey)
                return $ (user DBEsq.^. SCH.UserProfileName,
                          user DBEsq.^. SCH.UserMainEmail,
                          user DBEsq.^. SCH.UserPassword,
                          user DBEsq.^. SCH.UserSecret,
                          user DBEsq.^. SCH.UserSecretExp)))
  case secret of
    [] -> return (Nothing, Nothing, Nothing, Nothing, Nothing)
    [(DBEsq.Value profName, DBEsq.Value email, DBEsq.Value pass, DBEsq.Value sec, DBEsq.Value exp)] -> 
      return (Just $ TE.decodeUtf8 profName, Just $ TE.decodeUtf8 email, Just pass, fmap (TE.decodeUtf8 . B64.decodeLenient) sec, exp) 
                 





-- Given a User's profName return his id, password and main email.
getUserPassByProfNameOrMainEmail :: MonadIO m => T.Text
                                              -> ReaderT MySQL.SqlBackend m (Maybe (Integer, T.Text, ByteString, T.Text))
getUserPassByProfNameOrMainEmail identifier = do
  password <- (DBEsq.select $ DBEsq.from (\(user) -> do
                 (DBEsq.limit 1)                            
                 DBEsq.where_ (          user DBEsq.^. SCH.UserProfileName 
                                         DBEsq.==. DBEsq.val (TE.encodeUtf8 identifier)
                               DBEsq.||. user DBEsq.^. SCH.UserMainEmail
                                         DBEsq.==. DBEsq.val (TE.encodeUtf8 identifier)  )
                 return $ (user DBEsq.^. SCH.UserId,
                           user DBEsq.^. SCH.UserProfileName,
                           user DBEsq.^. SCH.UserPassword,
                           user DBEsq.^. SCH.UserMainEmail)))
  case password of
    [] -> return Nothing
    [(DBEsq.Value userKey, DBEsq.Value profName, DBEsq.Value pass, DBEsq.Value mEmail)] -> 
      let userId = toInteger $ MySQL.unSqlBackendKey (SCH.unUserKey userKey) 
      in return $ Just (userId, TE.decodeUtf8 profName, pass, TE.decodeUtf8 mEmail)




-- Given a User's profName return his id, recovery code and recovery code expiration
getUserRecCode :: MonadIO m => T.Text
                            -> ReaderT MySQL.SqlBackend m (Maybe Integer, Maybe ByteString, Maybe TM.UTCTime)
getUserRecCode profName = do
  recCode <- (DBEsq.select $ DBEsq.from (\(user) -> do
                (DBEsq.limit 1)                            
                DBEsq.where_ ( user DBEsq.^. SCH.UserProfileName 
                               DBEsq.==. DBEsq.val (TE.encodeUtf8 profName) )
                return $ (user DBEsq.^. SCH.UserId,
                          user DBEsq.^. SCH.UserRecCode,
                          user DBEsq.^. SCH.UserRecCodeExp)))
  case recCode of
    [] -> return (Nothing, Nothing, Nothing)
    [(DBEsq.Value userKey, DBEsq.Value code, DBEsq.Value exp)] -> 
      let userId = toInteger $ MySQL.unSqlBackendKey (SCH.unUserKey userKey) 
      in return (Just userId, code, exp)





-- Given a User's profName, this function returns a tuple of the form (id, blocked, canCreate, secret, expiration)
-- The secret returned by this function is the decoded Base64 one.
getAdminValidationData :: MonadIO m => Integer
                                    -> ReaderT MySQL.SqlBackend m (Maybe Bool, Maybe Int, Maybe T.Text, Maybe TM.UTCTime)
getAdminValidationData userId = do
  let userKey =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  secret <- (DBEsq.select $ DBEsq.from (\(user) -> do
                (DBEsq.limit 1)                            
                DBEsq.where_ (user DBEsq.^. SCH.UserId
                              DBEsq.==. DBEsq.val userKey)
                return $ (user DBEsq.^. SCH.UserBlockedAdmin,
                          user DBEsq.^. SCH.UserCanCreate,
                          user DBEsq.^. SCH.UserSecret,
                          user DBEsq.^. SCH.UserSecretExp)))
  case secret of
    [] -> return (Nothing, Nothing, Nothing, Nothing)
    [(DBEsq.Value blocked, DBEsq.Value canCreate, DBEsq.Value sec, DBEsq.Value exp)] ->
      return (Just blocked, Just canCreate, fmap (TE.decodeUtf8 . B64.decodeLenient) sec, exp)



        
-- Given a userId, delete that user.
deleteUser :: MonadIO m => Integer -> ReaderT MySQL.SqlBackend m ()
deleteUser userId = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  Per.delete key  
  return ()
  



{-
  Given a userId and data to be updated in that user, this function
  updates that user's basic data if he exists.
  Be aware of uniqueness constraints in User Model.
-}
updateUserData :: MonadIO m => Integer
                            -> UserData
                            -> ReaderT MySQL.SqlBackend m ()
updateUserData userId userData = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  Per.update key  [SCH.UserProfileName Per.=. TE.encodeUtf8 profName,
                   SCH.UserName Per.=. fName,
                   SCH.UserLastName Per.=. lName,
                   SCH.UserBirthDate Per.=. bDate,
                   SCH.UserCountry Per.=. country,
                   SCH.UserGender Per.=. gen,
                   SCH.UserMainEmail Per.=. TE.encodeUtf8 mEmail,
                   SCH.UserSecondEmail Per.=. maybeEmail,
                   SCH.UserPhoneNum Per.=. maybePhone]
  return ()  
  where
    ( profName, fName, lName, bDate, country,
      gen, mEmail, maybeEmail, maybePhone  ) = userData
   




-- Check if the given profileName is available in DB.
profileNameAvailability :: MonadIO m => T.Text
                                     -> ReaderT MySQL.SqlBackend m Bool
profileNameAvailability profName = do
  userCount <- Per.count [SCH.UserProfileName Per.==. TE.encodeUtf8 profName]
  if userCount == 0
     then return True
     else return False





-- Check if the given email is available in DB.
emailAvailability :: MonadIO m => T.Text
                               -> ReaderT MySQL.SqlBackend m Bool
emailAvailability email = do
  userCount <- Per.count [SCH.UserMainEmail Per.==. TE.encodeUtf8 email]
  if userCount == 0
     then return True
     else return False




-- Update the user's password.                                               
updatePassword :: MonadIO m => Integer
                            -> T.Text
                            -> ReaderT MySQL.SqlBackend m ()
updatePassword userId newPass = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  hashedPass <- liftIO $ makePassword (TE.encodeUtf8 newPass) 17 
  Per.update key [SCH.UserPassword Per.=. hashedPass]
          



-- Given a userId, an imgName and a binary blob, this function stores the user
-- avatar in DB and returns the id assigned to it.
storeUserAvatar :: MonadIO m => Integer
                             -> ByteString
                             -> ReaderT MySQL.SqlBackend m T.Text
storeUserAvatar userId blob = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  avatarId <- liftIO $ genAvatarId 
  let avatar = SCH.ProfileAvatar avatarId key blob
  Per.insert avatar
  return $ TE.decodeUtf8 avatarId




-- Update a previously stored avatar blob.
updateUserAvatar :: MonadIO m => Integer
                              -> ByteString
                              -> ReaderT MySQL.SqlBackend m T.Text
updateUserAvatar userId blob = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  avatarId <- liftIO $ genAvatarId 
  Per.updateWhere [SCH.ProfileAvatarUserId Per.==. key]
                  [SCH.ProfileAvatarAvatarId Per.=. avatarId,
                   SCH.ProfileAvatarImage Per.=. blob]
  return $ TE.decodeUtf8 avatarId




-- Given an userId, update his status as an admin, that is, whether he is blocked or not.
updateAdminBlockedStatus :: MonadIO m => Integer
                                      -> Bool
                                      -> ReaderT MySQL.SqlBackend m ()
updateAdminBlockedStatus userId status = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  Per.update key [SCH.UserBlockedAdmin Per.=. status]
  return ()

  
-- Given an userId, decrement by 1 the number of units he can create.
updateUserCanCreate :: MonadIO m => Integer
                                 -> ReaderT MySQL.SqlBackend m ()
updateUserCanCreate userId = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  Per.update key [SCH.UserCanCreate Per.-=. 1]
  return ()


-- Given the idCode of a user, this function deletes his avatar.
deleteUserAvatar :: MonadIO m => Integer
                              -> ReaderT MySQL.SqlBackend m ()
deleteUserAvatar userId = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  Per.deleteBy $ SCH.UniqueAvatar key
  return ()




-- Given a user's idCode, this function updates that user's secret.
-- It is important to note that this function stores a base64 encoding of
-- the secret in DB. However, the original secret is returned.
updateUserSecret :: MonadIO m => Integer
                              -> ReaderT MySQL.SqlBackend m T.Text
updateUserSecret userId = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  currentDate <- liftIO TM.getCurrentTime
  secret <- liftIO $ genSecret userId
  Per.update key [SCH.UserSecret Per.=. (Just $ B64.encode secret),
                  SCH.UserSecretExp Per.=. (Just $ TM.addUTCTime  691200 currentDate)]
  return $ TE.decodeUtf8 secret
  



-- Given a user's idCode, this function invalidates that user's secret.
-- It just updates the secret filed to a null value.
invalidateUserSecret :: MonadIO m => Integer
                                  -> ReaderT MySQL.SqlBackend m ()
invalidateUserSecret userId = do
  let key =  SCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  Per.update key [SCH.UserSecret Per.=. Nothing,
                  SCH.UserSecretExp Per.=. Nothing] -- token expires 8 days after creation
 







