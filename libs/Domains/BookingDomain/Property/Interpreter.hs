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

module Domains.BookingDomain.Property.Interpreter ( propertyServer ) where


import Data.ByteString.Char8 (pack)
import Control.Monad.Except
import Network.Wai
import Servant
import Servant.JuicyPixels
import Data.Aeson
import Data.Aeson.Types
import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Domains.BookingDomain.Property.API
import Configs.ConfigTypes
import HelperLibs.MySQL.ActionRunner
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad (mapM)
import Servant.JuicyPixels
import HelperLibs.Interpreters.Property
import HelperLibs.Interpreters.BookingDomain
import HelperLibs.ElasticSearch.ResponseParser
import qualified Repositories.UserRepo.Operations as UR (updateUserCanCreate)
import qualified Data.ByteString.Base64 as B64
import qualified Database.Persist.MySQL as MySQL
import qualified Data.CountryCodes as CC
import qualified Repositories.PropertyRepo.Operations as PR
import qualified Repositories.BookableRepo.Operations as BR
import qualified Domains.BookingDomain.Property.DataTypes as PT
import qualified Domains.BookingDomain.Bookable.DataTypes as BT
import qualified Data.ByteString.Lazy as LB (toStrict, fromStrict)
import qualified Data.ByteString as SB (length, ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE



-- Given the basic data to create a property, index a property in ElasticSearch.
-- That property is related to a UserId. Thus, the user who wants to create property
-- has to pass some vlidation.

postProperty :: ConfigMySQL
             -> ConfigES
             -> Maybe T.Text
             -> PT.BasicPropData
             -> ExceptT ServantErr IO PT.Property 
postProperty _ _ Nothing _ = throwError _noToken
postProperty coMysql coEs (Just token) propData = do
  propCreation <- liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdmin token
    case adminCreds of
      Left error -> return $ Left error 
      Right (userId, canCreate) -> do
        case canCreate < 1 of
          True -> return cannotCreate
          False -> do
            case validateBasicPropData propData of
              Left error -> return $ Left error
              Right validatedData -> do
                let PT.BasicPropData propName propType location = validatedData
                    PT.Location cCode region city address zipCode = location
                TM.UTCTime currentDate _ <- liftIO TM.getCurrentTime
                jsonRes <- liftIO $ runExceptT $ PR.indexProperty coEs
                                                                  userId
                                                                  (T.pack $ show currentDate)
                                                                  propName
                                                                  propType
                                                                  (CC.toText cCode, region, city, address, zipCode) 
                case jsonRes of
                  Left error -> return indexPropErr 
                  Right (Object object) -> do
                    UR.updateUserCanCreate userId
                    return $ maybe propParsingErr
                                   (\(String propId) -> Right (propId, (TM.UTCTime currentDate 0)))
                                   (parseMaybe (object .:) "_id")
  case getCustomError propCreation of
    Left error -> throwError error
    Right (propId, regDate) -> return $ PT.Property propId
                                                    propData
                                                    regDate
                                                    Nothing
                                                    Nothing
                                                    []
                                                    []
                                                    []
                                                    Nothing



-- Given a propertyId get that property.
getProperty :: ConfigMySQL
            -> ConfigES
            -> T.Text
            -> ExceptT ServantErr IO PT.PropertyWithImgIds
getProperty coMysql coEs propId = do
  property <- liftIO $ runMySQL coMysql $ do
    maybeProperty <- liftIO $ queryAndParseEitherProperty coEs propId
    case maybeProperty of
      Left error -> return $ Left error
      Right prop -> do
        imgIds <- PR.propertyImageIds propId
        return $ Right (PT.PropertyWithImgIds prop imgIds)
  either (\err -> throwError err) (\val -> return val) (getCustomError property)



-- Given a Region data type update the location of a Property.
updatePropertyLocation :: ConfigMySQL
                       -> ConfigES
                       -> T.Text
                       -> Maybe T.Text
                       -> PT.Region
                       -> ExceptT ServantErr IO ()
updatePropertyLocation _ _ _ Nothing _ = throwError _noToken                       
updatePropertyLocation coMysql coEs propId (Just token) region = do 
  update <- liftIO $ updateProperty coMysql coEs propId token region validateRegionData PR.updatePropertyLoc
  case getCustomError update of
    Left error -> throwError error
    Right () -> updatePropertyBooks coEs propId region updateLoc
  where
    updateLoc config (PT.Region r c _ _) (BT.Bookable bookId cCode _ _ _) = do
      BR.updateBookablePropLoc config bookId (CC.toText cCode, r, c)

      
      
-- Given a property name, update a property's name.
updatePropertyName :: ConfigMySQL
                   -> ConfigES
                   -> T.Text
                   -> Maybe T.Text
                   -> T.Text
                   -> ExceptT ServantErr IO ()
updatePropertyName _ _ _ Nothing _ = throwError _noToken
updatePropertyName coMysql coEs propId (Just token) name = do
  update <- liftIO $ updateProperty coMysql coEs propId token name validatePropName PR.updatePropertyName
  either (\err -> throwError err) (\val -> return ()) (getCustomError update)



-- Given a property type, update a property's type.
updatePropertyType :: ConfigMySQL
                   -> ConfigES
                   -> T.Text
                   -> Maybe T.Text
                   -> T.Text
                   -> ExceptT ServantErr IO ()
updatePropertyType _ _ _ Nothing _ = throwError _noToken
updatePropertyType coMysql coEs propId (Just token) propType = do
  update <- liftIO $ updateProperty coMysql coEs propId token propType validatePropType PR.updatePropertyType
  case getCustomError update of
    Left error -> throwError error
    Right () -> updatePropertyBooks coEs propId propType updateType
    where
    updateType config t (BT.Bookable bookId _ _ _ _) = do
      BR.updateBookablePropType config bookId t



-- Given a description, update a property's esDescription.
updatePropertyEsDesc :: ConfigMySQL
                     -> ConfigES
                     -> T.Text
                     -> Maybe T.Text
                     -> T.Text
                     -> ExceptT ServantErr IO ()
updatePropertyEsDesc _ _ _ Nothing _ = throwError _noToken
updatePropertyEsDesc coMysql coEs propId (Just token) esDesc = do
  update <- liftIO $ updateProperty coMysql coEs propId token esDesc validateDescription PR.updatePropertyEsDesc
  either (\err -> throwError err) (\val -> return ()) (getCustomError update)





-- Given a description, update a property's enDescription.
updatePropertyEnDesc :: ConfigMySQL
                     -> ConfigES
                     -> T.Text
                     -> Maybe T.Text
                     -> T.Text
                     -> ExceptT ServantErr IO ()
updatePropertyEnDesc _ _ _ Nothing _ = throwError _noToken
updatePropertyEnDesc coMysql coEs propId (Just token) enDesc = do
  update <- liftIO $ updateProperty coMysql coEs propId token enDesc validateDescription PR.updatePropertyEnDesc
  either (\err -> throwError err) (\val -> return ()) (getCustomError update)




-- Given a list of facilities, update a property's facilities.
updatePropertyFacs :: ConfigMySQL
                   -> ConfigES
                   -> T.Text
                   -> Maybe T.Text
                   -> [T.Text]
                   -> ExceptT ServantErr IO ()
updatePropertyFacs _  _  _ Nothing _ = throwError _noToken
updatePropertyFacs coMysql coEs propId (Just token) facs = do
  update <- liftIO $ updateProperty coMysql coEs propId token facs validateFacilities PR.updatePropertyFacs
  case getCustomError update of
    Left error -> throwError error
    Right () -> updatePropertyBooks coEs propId facs updateFacs
   where
    updateFacs config fs (BT.Bookable bookId _ _ _ _) = do
      BR.updateBookablePropFacs config bookId fs




-- Given a list of rules, update a property's rules.
updatePropertyRules :: ConfigMySQL
                    -> ConfigES
                    -> T.Text
                    -> Maybe T.Text
                    -> [T.Text]
                    -> ExceptT ServantErr IO ()
updatePropertyRules _ _ _ Nothing _ = throwError _noToken 
updatePropertyRules coMysql coEs propId (Just token) rules = do
  update <- liftIO $ updateProperty coMysql coEs propId token rules validateRules PR.updatePropertyRules
  either (\err -> throwError err) (\val -> return ()) (getCustomError update)




-- Given a list of phones, update a propery's phones.
updatePropertyPhones :: ConfigMySQL
                     -> ConfigES
                     -> T.Text
                     -> Maybe T.Text
                     -> [T.Text]
                     -> ExceptT ServantErr IO ()
updatePropertyPhones _  _  _ Nothing _ = throwError _noToken
updatePropertyPhones coMysql coEs propId (Just token) phones = do
  update <- liftIO $ updateProperty coMysql coEs propId token phones validatePhones PR.updatePropertyPhones
  either (\err -> throwError err) (\val -> return ()) (getCustomError update)




-- Given the id of an image, update that property's main image id.
updatePropertyMainImgId :: ConfigMySQL
                        -> ConfigES
                        -> T.Text
                        -> T.Text
                        -> Maybe T.Text
                        -> ExceptT ServantErr IO ()
updatePropertyMainImgId _ _ _ _ Nothing = throwError _noToken
updatePropertyMainImgId coMysql coEs propId mainImgId (Just token) = do 
  update <- liftIO $ updateProperty coMysql coEs propId token (Just mainImgId) validateImgId PR.assignMainPropImg
  either (\err -> throwError err) (\val -> return ()) (getCustomError update)



-- Given a DynamicImage (JPEG), store that image in DB and generate an id for that image.
-- Note that no more than 30 images can be stored per property.
postPropertyImg :: ConfigMySQL
                -> ConfigES
                -> T.Text
                -> Maybe T.Text
                -> DynamicImage 
                -> ExceptT ServantErr IO T.Text
postPropertyImg _ _ _ Nothing _ = throwError _noToken
postPropertyImg coMysql coEs propId (Just token) (ImageYCbCr8 imgJPEG) = do
  postImg <- liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdminAndMatchProperty coEs token propId
    case adminCreds of
      Left error -> return $ Left error
      Right _ -> do
        imgCount <- PR.propertyImageCount propId
        if imgCount >= 30 
        then return tooMuchImgs
        else do 
          let encodedImg = LB.toStrict (encodeJpegAtQuality 100 imgJPEG)
              numBytes = SB.length encodedImg
          case checkImgBound numBytes of
            LT -> return tooSmallImage
            GT -> return tooLargeImage
            EQ -> do
              imgId <- PR.storePropertyImg propId encodedImg
              return $ Right imgId
  either (\err -> throwError err) (\v -> return v) (getCustomError postImg)
  


-- Given a propId and a imgId, delete that property's image. 
deletePropertyImg :: ConfigMySQL
                  -> ConfigES
                  -> T.Text
                  -> T.Text
                  -> Maybe T.Text
                  -> ExceptT ServantErr IO ()
deletePropertyImg _ _ _ _ Nothing = throwError _noToken                 
deletePropertyImg coMysql coEs propId imgId (Just token) = do
  delImg <- liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdminAndMatchProperty coEs token propId
    case adminCreds of
      Left error -> return $ Left error
      Right (_, _, mainImgId, _, _, _, _, _) -> do
        PR.deletePropertyImg propId imgId
        case mainImgId of
          Nothing -> return $ Right ()
          Just imgId' ->
            if imgId == imgId'
            then do
              jsonRes <- liftIO $ runExceptT $ PR.assignMainPropImg coEs propId Nothing   
              return $ either (\err -> updatePropErr) (\val -> Right ()) jsonRes
            else return $ Right ()
  either (\err -> throwError err) (\v -> return v) (getCustomError delImg)
 



-- Given a propertyId, get the Ids of all that property's images.
getPropertyImageIds :: ConfigMySQL
                    -> T.Text
                    -> ExceptT ServantErr IO [T.Text]
getPropertyImageIds coMysql propId = do
  imgIds <- liftIO $ runMySQL coMysql $ PR.propertyImageIds propId
  return imgIds




-- Get all properties with pagination (from, size). In case pagination is not provided, the default is (0, 15).
getProperties :: ConfigES
              -> Maybe Int
              -> Maybe Int
              -> ExceptT ServantErr IO [PT.Property]
getProperties config from size = do
  jsonRes <- liftIO $ runExceptT $ PR.queryProperties config from size
  case jsonRes of
    Left _ -> throwError _searchPropErr
    Right (Object object) -> maybe (throwError _propParsingErr)
                                   (\properties -> return properties)
                                   (parseProperties object)
   



-- Get all the properties belonging to an admin.
getAdminProperties :: ConfigMySQL
                   -> ConfigES
                   -> Maybe T.Text
                   -> ExceptT ServantErr IO [PT.Property]
getAdminProperties _  _ Nothing = throwError _noToken                   
getAdminProperties coMysql coEs (Just token) = do
  properties <- liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdmin token
    case adminCreds of
      Left error -> return $ Left error
      Right (userId, _) -> do
        jsonRes <- liftIO $ runExceptT $ PR.queryPropertiesByAdmin coEs userId
        case jsonRes of
          Left _ -> return $ searchPropErr
          Right (Object object) ->
            return $ maybe propParsingErr (\props -> Right props) (parseProperties object)
  either (\err -> throwError err) (\v -> return v) (getCustomError properties)      
  



-- Get all properties according to a country code and a location string which may indicate a region-city name, or
-- the specific name of a property.
getPropertiesByLocOrName :: ConfigES
                         -> T.Text
                         -> Maybe T.Text
                         -> Maybe Int
                         -> Maybe Int
                         -> ExceptT ServantErr IO [PT.Property]
getPropertiesByLocOrName config cCode query from size = do
  jsonRes <- liftIO $ runExceptT $ PR.queryPropertiesByLocOrName config cCode (maybe "" id query) from size 
  case jsonRes of
    Left _ -> throwError _searchPropErr
    Right (Object object) ->
      maybe (throwError _propParsingErr) (\props -> return props) (parseProperties object) 



  
propertyServer :: ConfigMySQL -> ConfigES -> Server PropertyAPI
propertyServer coMysql coEs = (getProperty coMysql coEs)
                         :<|> (postProperty coMysql coEs)  
                         :<|> (updatePropertyLocation coMysql coEs)
                         :<|> (updatePropertyName coMysql coEs)
                         :<|> (updatePropertyType coMysql coEs)
                         :<|> (updatePropertyEsDesc coMysql coEs)         
                         :<|> (updatePropertyEnDesc coMysql coEs)         
                         :<|> (updatePropertyFacs coMysql coEs)
                         :<|> (updatePropertyRules coMysql coEs)
                         :<|> (updatePropertyPhones coMysql coEs)
                         :<|> (updatePropertyMainImgId coMysql coEs)
                         :<|> (postPropertyImg coMysql coEs)
                         :<|> (deletePropertyImg coMysql coEs)
                         :<|> (getPropertyImageIds coMysql)
                         :<|> (getProperties coEs)
                         :<|> (getAdminProperties coMysql coEs)
                         :<|> (getPropertiesByLocOrName coEs)
