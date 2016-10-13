{-# LANGUAGE OverloadedStrings     #-}

module Repositories.PropertyRepo.Operations where

import Configs.ConfigTypes
import HelperLibs.ElasticSearch.BodyBuilder
import HelperLibs.ElasticSearch.Request
import HelperLibs.ElasticSearch.PropertyRepo
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import HelperLibs.ElasticSearch.ResponseParser
import Control.Monad.Except
import Data.ByteString (ByteString)
import Control.Monad.Trans.Reader
import qualified Data.Time as TM
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Simple as SHTTP
import qualified Schemas.SQL.PropertyImage as SCH
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Database.Persist as Per
import qualified Database.Esqueleto  as DBEsq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE



-- Given an adminId and the basic information to create a property, index a document
-- with that property's basic info.
indexProperty :: ConfigES
              -> Integer
              -> T.Text
              -> T.Text
              -> T.Text
              -> (T.Text, T.Text, T.Text, T.Text, Maybe T.Text)
              -> ExceptT String IO Value
indexProperty config adminId regDate propName propType loc  = ExceptT $ do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 document) :: Either String Value
  case mQueryBody of
    Left err -> return $ Left err
    Right body -> do
      let makeReq =  (partialRequest "POST" port host $ "/" ++ iName ++  "/api_property/")
                   . (SHTTP.setRequestBodyJSON body)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        201 -> do
          let resBody = SHTTP.getResponseBody response
          return $ Right (resBody :: Value)
        _  -> return $ Left "Property couldn't be indexed"   
  where
    (ConfigES host port iName _ _ _ _) = config
    (country, region, city, address, maybeZip) = loc
    zipCode = case maybeZip of
      Nothing -> nullVal "zip_code"
      Just z -> strVal ("zip_code", z)
    document = objVal [intVal ("admin_id", adminId),
                       strVal ("prop_name", propName),
                       strVal ("lodging_type", propType),
                       strVal ("country_code", country),
                       strVal ("region", region),
                       strVal ("city", city),
                       strVal ("address", address),
                       zipCode,
                       strVal ("reg_date", regDate)]
                       


-- Find a property by Id.
queryPropertyById :: ConfigES
                  -> T.Text
                  -> ExceptT String IO Value
queryPropertyById config propId = ExceptT $ do
  let makeReq = partialRequest "GET" port host ("/" ++ iName ++ "/api_property/" ++ (T.unpack propId))
  response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
  logResponse response
  case SHTTP.getResponseStatusCode response of
    200 -> do
      let resBody = SHTTP.getResponseBody response
      return $ Right (resBody :: Value)
    _  -> return $ Left ("Property with id " ++ (T.unpack propId) ++ " was not found")
  where
    (ConfigES host port iName _ _ _ _) = config





-- Find a Property's images and main image.
propertyImageIds :: MonadIO m => T.Text
                              -> ReaderT MySQL.SqlBackend m [T.Text]
propertyImageIds propId = do
  ids <- (DBEsq.select $ DBEsq.from (\(propImg) -> do
            DBEsq.where_ (propImg DBEsq.^. SCH.PropertyImagePropertyId
                          DBEsq.==. DBEsq.val (TE.encodeUtf8 propId))
            return $ propImg DBEsq.^. SCH.PropertyImageImageId))
  return $ fmap (\(DBEsq.Value key) -> TE.decodeUtf8 key) ids





-- Delete a property by Id.
deleteProperty :: ConfigES
               -> T.Text
               -> IO Value
deleteProperty config propId = do
  let makeReq = partialRequest "DELETE" port host ("/" ++ iName ++ "/api_property/" ++ (T.unpack propId))
  response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
  logResponse response
  let resBody = SHTTP.getResponseBody response
  return resBody
  where
    (ConfigES host port iName _ _ _ _) = config
   



-- Update a Property's location
updatePropertyLoc :: ConfigES
                  -> T.Text
                  -> (T.Text, T.Text, T.Text, Maybe T.Text)
                  -> ExceptT String IO Value
updatePropertyLoc config propId loc = ExceptT $ updateProperty host port iName propId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    (region, city, address, maybeZip) = loc
    zipCode = case maybeZip of
      Nothing -> nullVal "zip_code"
      Just z -> strVal ("zip_code", z)
    updates = objStr ("doc", [strVal ("region", region),
                              strVal ("city", city),
                              strVal ("address", address),
                              zipCode])         
   
-- Update a Property's name
updatePropertyName :: ConfigES
                   -> T.Text
                   -> T.Text
                   -> ExceptT String IO Value
updatePropertyName config propId propName = ExceptT $ updateProperty host port iName propId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [strVal ("prop_name", propName)])
                              



-- Update a Property's type 
updatePropertyType :: ConfigES
                   -> T.Text
                   -> T.Text
                   -> ExceptT String IO Value
updatePropertyType config propId propType = ExceptT $ updateProperty host port iName propId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [strVal ("lodging_type", propType)])




-- Update a Property's description in English
updatePropertyEnDesc :: ConfigES
                     -> T.Text
                     -> T.Text
                     -> ExceptT String IO Value
updatePropertyEnDesc config propId desc = ExceptT $ updateProperty host port iName propId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [strVal ("eng_prop_desc", desc)])




-- Update a Property's description in Spanish
updatePropertyEsDesc :: ConfigES
                     -> T.Text
                     -> T.Text
                     -> ExceptT String IO Value
updatePropertyEsDesc config propId desc = ExceptT $ updateProperty host port iName propId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [strVal ("esp_prop_desc", desc)])




-- Update a Property's Facilities 
updatePropertyFacs :: ConfigES
                     -> T.Text
                     -> [T.Text]
                     -> ExceptT String IO Value
updatePropertyFacs config propId facilities = ExceptT $ updateProperty host port iName propId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [arrVal ("facilities", facilities)])


-- Update a Property's Rules
updatePropertyRules :: ConfigES
                    -> T.Text
                    -> [T.Text]
                    -> ExceptT String IO Value
updatePropertyRules config propId rules = ExceptT $ updateProperty host port iName propId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [arrVal ("rules", rules)])




-- Update a Property's Contact Phones
updatePropertyPhones :: ConfigES
                     -> T.Text
                     -> [T.Text]
                     -> ExceptT String IO Value
updatePropertyPhones config propId phones = ExceptT $ updateProperty host port iName propId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [arrVal ("contact_nums", phones)])



-- Given a propertyId, an imgName and a binary blob, this function stores a property's
-- image in DB and returns the id assigned to it.
storePropertyImg :: MonadIO m => T.Text
                              -> ByteString
                              -> ReaderT MySQL.SqlBackend m T.Text
storePropertyImg propId blob = do
  imgId <- liftIO $ genPropImgId propId 
  let image = SCH.PropertyImage imgId (TE.encodeUtf8 propId) blob
  Per.insert image
  return $ TE.decodeUtf8 imgId




-- Given a propertyId and an ImgId this function assigns that image as the main image.

assignMainPropImg :: ConfigES
                  -> T.Text
                  -> Maybe T.Text
                  -> ExceptT String IO Value
assignMainPropImg config propId maybeImgId = ExceptT $ updateProperty host port iName propId updates
  where
    imgId = case maybeImgId of
      Nothing -> nullVal "main_img_id"
      Just v -> strVal ("main_img_id", v)
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [imgId])


  

-- Given a ImgId, this function deletes that image
deletePropertyImg :: MonadIO m => T.Text
                               -> T.Text
                               -> ReaderT MySQL.SqlBackend m ()
deletePropertyImg propId imgId = do
  Per.deleteWhere [SCH.PropertyImageImageId Per.==. (TE.encodeUtf8 imgId),
                   SCH.PropertyImagePropertyId Per.==. (TE.encodeUtf8 propId)]
  return ()



-- Get the count of images belonging to a given property.
propertyImageCount :: MonadIO m => T.Text
                                -> ReaderT MySQL.SqlBackend m Int
propertyImageCount propId = do
  imgCount <- Per.count [SCH.PropertyImagePropertyId Per.==. TE.encodeUtf8 propId]
  return imgCount 



-- Query properties within a given boundry.
queryProperties :: ConfigES
                -> Maybe Int
                -> Maybe Int
                -> ExceptT String IO Value
queryProperties config mFrom mSize = ExceptT $ propertyQuery host port iName query   
  where
    (ConfigES host port iName _ _ _ _) = config
    from = setBound mFrom 0
    size = setBound mSize 15
    query = objVal [objStr ("query" , [objStr ("match_all", [])]),
                                       intVal ("from", from),
                                       intVal ("size", size)]




-- Query all the properties which match a given Id.
queryPropertiesByAdmin :: ConfigES
                       -> Integer
                       -> ExceptT String IO Value
queryPropertiesByAdmin config adminId = ExceptT $ propertyQuery host port iName query
  where
    (ConfigES host port iName _ _ _ _) = config      
    filt = objStr ("must" , [objStr ("term", [intVal ("admin_id", adminId)])])     
    filters = objStr ("filter", [objStr ("bool", [filt])])   
    query = braces $ objStr ("query", [objStr ("bool", [filters])])
 


-- Query properties with a search that filters country and matches either region, city or 
-- property name.
queryPropertiesByLocOrName :: ConfigES
                            -> T.Text
                            -> T.Text
                            -> Maybe Int
                            -> Maybe Int
                            -> ExceptT String IO Value
queryPropertiesByLocOrName config cCode locStr mFrom mSize = ExceptT $ propertyQuery host port iName query
  where
    (ConfigES host port iName _ _ _ _) = config
    from = setBound mFrom 0
    size = setBound mSize 15
    should = arrObj ("should", [braces $ objStr ("match", [strVal ("region", locStr)]),
                                braces $ objStr ("match", [objStr ("city", [strVal ("query", locStr),
                                                                                 intVal ("boost", 2) ])]),
                                braces $ objStr ("match", [objStr ("prop_name", [strVal ("query", locStr),
                                                                                 intVal ("boost", 3) ])]) ]) -- boost city and names   
    filt = arrObj ("must", [braces $ objStr ("term", [strVal ("country_code", cCode)])])      
    filters = objStr ("filter", [objStr ("bool", [filt])])   
    query =  objVal [objStr ("query", [objStr ("bool", [should,
                                                        filters,
                                                        intVal ("minimum_should_match", 0) ])]),
                     intVal ("from", from),
                     intVal ("size", size)]
  



