{-# LANGUAGE OverloadedStrings     #-}

module Repositories.BookableRepo.Operations where


import Configs.ConfigTypes
import HelperLibs.ElasticSearch.BodyBuilder
import HelperLibs.ElasticSearch.Request
import HelperLibs.ElasticSearch.ResponseParser
import HelperLibs.ElasticSearch.BookableRepo
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.ByteString (ByteString)
import Control.Monad.Trans.Reader
import qualified Data.Time as TM
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Simple as SHTTP
import qualified Schemas.SQL.BookableImage as SCH
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Database.Persist as Per
import qualified Database.Esqueleto  as DBEsq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


type BklSpecs = (T.Text, Maybe T.Text, Maybe T.Text, Int, [T.Text])
type PricingData = (T.Text, Int, [T.Text], Integer, Int)


-- Given an propertyId and the basic information to create a bookable, index a document
-- with that bookable's basic info. The status on creation is Unlisted.
indexBookable :: ConfigES
              -> T.Text
              -> BklSpecs 
              -> Maybe T.Text
              -> Maybe T.Text
              -> Int
              -> [T.Text]
              -> (T.Text, T.Text, T.Text)
              -> T.Text
              -> [T.Text]
              -> ExceptT String IO Value
indexBookable config propId bklSpecs mEsDesc mEnDesc maxOccu rooms propLoc propType propFacs = ExceptT $ do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 document) :: Either String Value
  case mQueryBody of
    Left err -> return $ Left err
    Right body -> do
      let makeReq =  (partialRequest "POST" port host $ "/" ++ iName ++  "/api_bookable/")
                   . (SHTTP.setRequestBodyJSON body)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        201 -> do
          let resBody = SHTTP.getResponseBody response
          return $ Right (resBody :: Value)
        _  -> return $ Left "Bookable couldn't be indexed"   
  where
    (ConfigES host port iName _ _ _ _) = config
    (cCode, region, city) = propLoc
    (bklName, mRoomSize, mBedType, bedNum, amenities) = bklSpecs
    document = objVal [strVal ("prop_id", propId),
                       strVal ("bkl_name", bklName),
                       setValOrNull "esp_bkl_desc" mEsDesc,
                       setValOrNull "eng_bkl_desc" mEnDesc,
                       setValOrNull "room_size" mRoomSize,
                       setValOrNull "bed_type" mBedType,
                       intVal ("bed_num", bedNum),
                       intVal ("max_occupancy", maxOccu),
                       arrVal ("amenities", amenities),
                       arrVal ("room_ids", rooms),
                       boolVal ("listed", False),
                       strVal ("prop_country_code", cCode),
                       strVal ("prop_region", region),
                       strVal ("prop_city", city),
                       strVal ("prop_type", propType),
                       arrVal ("prop_facilities", propFacs)]




-- Find a bookable by Id.
queryBookableById :: ConfigES
                  -> T.Text
                  -> ExceptT String IO Value
queryBookableById config bklId = ExceptT $ do
  let makeReq = partialRequest "GET" port host ("/" ++ iName ++ "/api_bookable/" ++ (T.unpack bklId))
  response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
  logResponse response
  case SHTTP.getResponseStatusCode response of
    200 -> do
      let resBody = SHTTP.getResponseBody response
      return $ Right (resBody :: Value)
    _  -> return $ Left ("Bookable with id " ++ (T.unpack bklId) ++ " was not found")
  where
    (ConfigES host port iName _ _ _ _) = config



-- Delete a bookable by Id.
deleteBookable :: ConfigES
               -> T.Text
               -> ExceptT String IO Value
deleteBookable config bklId = ExceptT $ do
  let makeReq = partialRequest "DELETE" port host ("/" ++ iName ++ "/api_bookable/" ++ (T.unpack bklId))
  response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
  logResponse response
  case SHTTP.getResponseStatusCode response of
    200 -> do
      let resBody = SHTTP.getResponseBody response
      return $ Right (resBody :: Value)
    _  -> return $ Left ("Bookable Was Not Deleted")
  where
    (ConfigES host port iName _ _ _ _) = config





-- Update a Bookable's state: listed or unlisted

updateBookableState :: ConfigES
                    -> T.Text
                    -> Bool
                    -> ExceptT String IO Value
updateBookableState config bklId state = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [boolVal ("listed", state)])
 




-- Update a Bookable's PropertyType

updateBookablePropType :: ConfigES
                       -> T.Text
                       -> T.Text
                       -> ExceptT String IO Value
updateBookablePropType config bklId propType = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [strVal ("prop_type", propType)])



-- Update a Bookable's Property's Location

updateBookablePropLoc :: ConfigES
                      -> T.Text
                      -> (T.Text, T.Text, T.Text)
                      -> ExceptT String IO Value
updateBookablePropLoc config bklId loc = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    (cCode, region, city) = loc
    updates = objStr ("doc", [strVal ("prop_country_code", cCode),
                              strVal ("prop_region", region),
                              strVal ("prop_city", city)])




-- Update a Bookable's Property's Facilities

updateBookablePropFacs :: ConfigES
                       -> T.Text
                       -> [T.Text]
                       -> ExceptT String IO Value
updateBookablePropFacs config bklId facs = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [arrVal ("prop_facilities", facs)])





-- Update a Bookable's description in English

updateBookableEnDesc :: ConfigES
                     -> T.Text
                     -> T.Text
                     -> ExceptT String IO Value
updateBookableEnDesc config bklId enDesc = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [strVal ("eng_bkl_desc", enDesc)])




-- Update a Bookable's description in Spanish

updateBookableEsDesc :: ConfigES
                     -> T.Text
                     -> T.Text
                     -> ExceptT String IO Value
updateBookableEsDesc config bklId esDesc = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [strVal ("esp_bkl_desc", esDesc)])




-- Update a Bookable's MaxOccupancy

updateBookableMaxOccu :: ConfigES
                      -> T.Text
                      -> Int
                      -> ExceptT String IO Value
updateBookableMaxOccu config bklId maxOccu = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [intVal ("max_occupancy", maxOccu)])





-- Update a Bookable's Specs

updateBookableSpecs :: ConfigES
                    -> T.Text
                    -> BklSpecs
                    -> ExceptT String IO Value
updateBookableSpecs config bklId bklSpecs = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    (bklName, mRoomSize, mBedType, bedNum, amenities) = bklSpecs
    updates = objStr ("doc", [strVal ("bkl_name", bklName),
                              setValOrNull "room_size" mRoomSize,
                              setValOrNull "bed_type" mBedType,
                              intVal ("bed_num", bedNum),
                              arrVal ("amenities", amenities)])



-- Update a Bookable's list of Rooms

updateBookableRoomIds :: ConfigES
                      -> T.Text
                      -> [T.Text]
                      -> ExceptT String IO Value
updateBookableRoomIds config bklId rooms = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    updates = objStr ("doc", [arrVal ("room_ids", rooms)])





-- Update a Bookable's list of pricings
updateBookablePricings :: ConfigES
                       -> T.Text
                       -> [PricingData]
                       -> ExceptT String IO Value
updateBookablePricings config bklId pricings = ExceptT $ updateBookable host port iName bklId updates   
  where
    (ConfigES host port iName _ _ _ _) = config
    jsonPris = fmap pricingDataToJSONstring pricings
    updates = objStr ("doc", [arrObj ("pricings", jsonPris)])




-- Given a bookableId, an imgName and a binary blob, this function stores a bookable's
-- image in DB and returns the id assigned to it.
storeBookableImg :: MonadIO m => T.Text
                              -> ByteString
                              -> ReaderT MySQL.SqlBackend m T.Text
storeBookableImg bklId blob = do
  imgId <- liftIO $ genBklImgId bklId 
  let image = SCH.BookableImage imgId (TE.encodeUtf8 bklId) blob
  Per.insert image
  return $ TE.decodeUtf8 imgId




-- Given a ImgId, this function deletes that image
deleteBookableImg :: MonadIO m => T.Text
                          -> T.Text
                          -> ReaderT MySQL.SqlBackend m ()
deleteBookableImg bklId imgId = do
  Per.deleteWhere [SCH.BookableImageImageId Per.==. (TE.encodeUtf8 imgId),
                   SCH.BookableImageBookableId Per.==. (TE.encodeUtf8 bklId)]
  return ()


-- Get the count of images belonging to a given bookable.
bookableImageCount :: MonadIO m => T.Text
                                -> ReaderT MySQL.SqlBackend m Int
bookableImageCount bklId = do
  imgCount <- Per.count [SCH.BookableImageBookableId Per.==. TE.encodeUtf8 bklId]
  return imgCount 




-- Find a Bookable's imageIds.
bookableImageIds :: MonadIO m => T.Text
                              -> ReaderT MySQL.SqlBackend m [T.Text]
bookableImageIds bklId = do
  ids <- (DBEsq.select $ DBEsq.from (\(bklImg) -> do
            DBEsq.where_ (bklImg DBEsq.^. SCH.BookableImageBookableId
                          DBEsq.==. DBEsq.val (TE.encodeUtf8 bklId))
            return $ bklImg DBEsq.^. SCH.BookableImageImageId))
  return $ fmap (\(DBEsq.Value key) -> TE.decodeUtf8 key) ids




-- Query Bookables belonging to a Property.
queryBookablesByProp :: ConfigES
                     -> T.Text
                     -> ExceptT String IO Value
queryBookablesByProp config propId = ExceptT $ bookableQuery host port iName query
  where
    (ConfigES host port iName _ _ _ _) = config      
    must = arrObj ("must", [braces $ objStr ("term", [strVal ("prop_id", propId)])])
    query = braces $ objStr ("query", [objStr ("bool", [must])])


  

-- Given a country code, a number of guests, and a destination (a query string either with a region or city)
-- perform a search. Country code and number of guests are filters. This query is partial
-- result for a basicSearch in the Bookable API.
basicBookableQuery :: ConfigES
                   -> T.Text
                   -> Int
                   -> T.Text
                   -> Int
                   -> Int
                   -> ExceptT String IO Value
basicBookableQuery config cCode guestNum locStr from size = ExceptT $ bookableQuery host port iName query   
  where
    (ConfigES host port iName _ _ _ _) = config
    should = arrObj ("should", [braces $ objStr ("match", [strVal ("prop_region", locStr)]),
                                braces $ objStr ("match", [objStr ("prop_city", [strVal ("query", locStr),
                                                                                 intVal ("boost", 3) ])]) ]) -- boost the city field      
    filt1 = arrObj ("must", [braces $ objStr ("term", [strVal ("prop_country_code", cCode)]),
                             braces $ objStr ("range", [objStr ("max_occupancy", [intVal ("gte", guestNum)])])])
    filt2 = objStr ("must_not" , [  objStr ("term", [boolVal ("listed", False)])])
    filters = objStr ("filter", [objStr ("bool", [filt1, filt2])])   
    query =  objVal [objStr ("query", [objStr ("bool", [should,
                                                        filters,
                                                        intVal ("minimum_should_match", 0) ])]),
                     intVal ("from", from),
                     intVal ("size", size)]



-- Given a country code, a number of guests, and a destination (a query string either with a region or city)
-- perform a search. Country code and number of guests are filters. This query is partial
-- result for a basicSearch in the Bookable API.
advancedBookableQuery :: ConfigES
                      -> T.Text
                      -> Int
                      -> T.Text
                      -> Maybe T.Text
                      -> [T.Text]
                      -> [T.Text]
                      -> Maybe Int
                      -> Maybe (Integer, Integer)
                      -> Maybe Int
                      -> Int
                      -> Int
                      -> ExceptT String IO Value
advancedBookableQuery config cCode guestNum locStr mPropType mPropFacs mAmens mBedNum mPriceRange mDiscount from size =
  ExceptT $ bookableQuery host port iName query   
  where
    (ConfigES host port iName _ _ _ _) = config
    facsMatch = case mPropFacs of
      [] -> Nothing
      facs -> Just $ braces $ objStr ("match", [strVal ("prop_facilities", makeTextQuery facs)])
    amensMatch = case mAmens of
      [] -> Nothing
      amens -> Just $ braces $ objStr ("match", [strVal ("amenities", makeTextQuery amens)])   
    priceFilter = case mPriceRange of
      Nothing -> Nothing 
      Just (lower, upper) -> Just $ braces $ objStr ("range", [objStr ("pricings.night_price", [intVal ("gte", lower),
                                                                                                intVal ("lte", upper) ])]) 
    bedFilter = case mBedNum of
      Nothing -> Nothing
      Just v ->  Just $ braces $ objStr ("range", [objStr ("bed_num", [intVal ("gte", v)])])
    propTypeFilter = case mPropType of
      Nothing -> Nothing
      Just v -> Just $ braces $ objStr ("term", [strVal ("prop_type", v)])
    discountFilter = case mDiscount of
      Nothing -> Nothing
      Just v -> Just $ braces $ objStr ("range", [objStr ("pricings.discount", [intVal ("gte", v)])])
    should = clauseQuery "should" [Just $ braces $ objStr ("match", [objStr ("prop_region", [strVal ("query", locStr),
                                                                                             intVal ("boost", 5) ])]),
                                   Just $ braces $ objStr ("match", [objStr ("prop_city", [strVal ("query", locStr),
                                                                                           intVal ("boost", 11) ])]),
                                   facsMatch,
                                   amensMatch]
    filt1 = clauseQuery "must" [Just $ braces $ objStr ("term", [strVal ("prop_country_code", cCode)]),
                                Just $ braces $ objStr ("range", [objStr ("max_occupancy", [intVal ("gte", guestNum)])]),
                                bedFilter,
                                propTypeFilter,
                                priceFilter,
                                discountFilter]  
    filt2 = objStr ("must_not" , [objStr ("term", [boolVal ("listed", False)])]) -- listed filter
    filters = objStr ("filter", [objStr ("bool", [filt1, filt2])])   
    query =  objVal [objStr ("query", [objStr ("bool", [should,
                                                        filters,
                                                        intVal ("minimum_should_match", 0) ])]),
                     intVal ("from", from),
                     intVal ("size", size)]





