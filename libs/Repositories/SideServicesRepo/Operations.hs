{-# LANGUAGE OverloadedStrings #-}

module Repositories.SideServicesRepo.Operations where


import Data.Time
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Crypto.PasswordStore
import HelperLibs.MySQL.SideServicesRepo
import qualified Schemas.SQL.User as USCH
import qualified Schemas.SQL.PropertyImage as PSCH
import qualified Schemas.SQL.BookableImage as BSCH
import qualified Schemas.SQL.DbTypes as DbT
import qualified Data.Time as TM
import qualified Schemas.SQL.Reservation as SCH
import qualified Database.Esqueleto  as DBEsq
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Persist as Per




-- Given an avatarId get the ByteString which encodes that image.
getUserAvatar :: MonadIO m => T.Text -> ReaderT MySQL.SqlBackend m (Either String ByteString) 
getUserAvatar avatarId = do
  maybeAvatar <- Per.getBy $ USCH.UniqueUserAvatar (TE.encodeUtf8 avatarId) 
  case maybeAvatar of
    Nothing -> return $ Left ("There's no user avatar with id " ++ (T.unpack avatarId))
    Just (Per.Entity key avatar) -> do
      let image = USCH.profileAvatarImage avatar
      return $ Right image




-- Given an propertyImageId get the ByteString which encodes that image.
getPropertyImage :: MonadIO m => T.Text -> ReaderT MySQL.SqlBackend m (Either String ByteString)
getPropertyImage imgId = do
  maybePropImg <- Per.getBy $ PSCH.UniquePropertyImage (TE.encodeUtf8 imgId) 
  case maybePropImg of
    Nothing -> return $ Left ("There's no property image with id " ++ (T.unpack imgId))
    Just (Per.Entity key propImg) -> do
      let image = PSCH.propertyImageImage propImg 
      return $ Right image

  


-- Given an bookableImgId get the ByteString which encodes that image.
getBookableImage :: MonadIO m => T.Text -> ReaderT MySQL.SqlBackend m (Either String ByteString)
getBookableImage imgId = do
  maybeBklImg <- Per.getBy $ BSCH.UniqueBookableImage (TE.encodeUtf8 imgId) 
  case maybeBklImg of
    Nothing -> return $ Left ("There's no bookable image with id " ++ (T.unpack imgId))
    Just (Per.Entity key bklImg) -> do
      let image = BSCH.bookableImageImage bklImg 
      return $ Right image



-- Given a userId this function generates a RecoveryCode for that user.
-- Note that this code expires after 1 hour.
-- Also note that the hashed version of the code is stored in DB. However,
-- the non-hashed version is the one that is returned by this function. The purpose
-- of this is that the API can email the non-hashed code to the user. 
updateRecoveryCode :: MonadIO m => Integer
                                -> ReaderT MySQL.SqlBackend m ByteString
updateRecoveryCode userId = do
  currentDate <- liftIO $ TM.getCurrentTime
  let expDate = TM.addUTCTime 900 currentDate -- Recovery codes expire 15 minutes after creation.
      key =  USCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  code <- liftIO $ genRecoveryCode
  hashedCode <- liftIO $ makePassword code 17
  Per.update key [USCH.UserRecCode Per.=. (Just hashedCode),
                  USCH.UserRecCodeExp Per.=. (Just expDate)]
  return code 



-- Given a user's idCode, this function invalidates that user's recCode.
invalidateUserRecCode :: MonadIO m => Integer
                                   -> ReaderT MySQL.SqlBackend m ()
invalidateUserRecCode userId = do
  let key = USCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  Per.update key [USCH.UserRecCode Per.=. Nothing,
                  USCH.UserRecCodeExp Per.=. Nothing] 





