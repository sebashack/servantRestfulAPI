{-# LANGUAGE OverloadedStrings     #-}

module HelperLibs.ElasticSearch.ResponseParser where


import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.CountryCodes as CC
import qualified Domains.BookingDomain.Property.DataTypes as PT
import qualified Domains.BookingDomain.Bookable.DataTypes as BT
import qualified Domains.ContentAdminDomain.Article.DataTypes as AT
import qualified Data.Time as TM
import qualified Network.HTTP.Simple as SHTTP
import qualified Data.ByteString.Lazy.Char8 as LC8



-- # Helper functions to parse ElasticSearch JSON Value Responses # --





-- Helper function to log the ElasticSearch responses
logResponse :: SHTTP.Response Value -> IO ()
logResponse res = do
  putStrLn $ "The status code: " ++ show (SHTTP.getResponseStatusCode res)
  print $ SHTTP.getResponseHeader "Content-Type" res
  LC8.putStrLn $ encode (SHTTP.getResponseBody res :: Value)




-- Some BASIC JSON Parsers --
parseIntAttr :: Integral a => Object -> T.Text -> Maybe a
parseIntAttr obj attr = do
  (Number v) <- parseMaybe (obj .:) attr
  return $ round v
  
parseBoolAttr :: Object -> T.Text -> Maybe Bool  
parseBoolAttr obj attr = do
  (Bool v) <- parseMaybe (obj .:) attr
  return v
  
parseStringAttr :: Object -> T.Text -> Maybe T.Text
parseStringAttr obj attr = do
  (String v) <- parseMaybe (obj .:) attr
  return v
  
parseOptionalStringAttr :: Object -> T.Text -> Maybe T.Text
parseOptionalStringAttr obj attr = do
  val <- parseMaybe (obj .:) attr
  case val of
    (String "null") -> Nothing 
    (String v) -> return v
    Null -> Nothing
    _ -> Nothing
    

parseArrString :: Object -> T.Text -> [T.Text]
parseArrString obj attr =
  case parseMaybe (obj .:) attr of
    Just (Array arr) -> V.toList $ fmap (\(String x) -> x) arr
    Just Null -> []
    Just (String "null") -> []
    _ -> []


parseDateAttr :: Object -> T.Text -> Maybe TM.UTCTime
parseDateAttr obj attr = do
  (String date) <- parseMaybe (obj .:) attr
  case T.split (\c -> c == '-') date of
    [year,month,day] -> do
      gregorian <- TM.fromGregorianValid (read $ T.unpack year) (read $ T.unpack month) (read $ T.unpack day)
      return $ TM.UTCTime gregorian 0 
    _ -> Nothing

    
-- END of BASIC JSON parsers --




-- This function receives an Object (json) and parses a Property if possible.      
parseProperty :: Object -> Maybe PT.Property
parseProperty object = do
  -- Obligatory Attributes
  _id <- parseStringAttr object "_id"
  _source <- parseMaybe (object .:) "_source" :: Maybe Object
  propType <- parseStringAttr _source "lodging_type"
  propName <- parseStringAttr _source "prop_name"
  cCode <- parseStringAttr _source "country_code"
  address <- parseStringAttr _source "address"
  region <- parseStringAttr _source "region"
  city <- parseStringAttr _source "city"
  regDate <- parseDateAttr _source "reg_date"
  -- Optional Attributes
  let zipCode = parseOptionalStringAttr _source "zip_code"
      esDesc = parseOptionalStringAttr _source "esp_prop_desc"
      enDesc = parseOptionalStringAttr _source "eng_prop_desc"
      facs = parseArrString _source "facilities"
      rules = parseArrString _source "rules"
      phNums = parseArrString _source "contact_nums"
      mainImgId = parseOptionalStringAttr _source "main_img_id"
  return $ PT.Property _id
                       (PT.BasicPropData propName propType (PT.Location (CC.fromText cCode) region city address zipCode))
                       regDate
                       esDesc
                       enDesc
                       facs
                       rules
                       phNums
                       mainImgId



-- Given an Object (json) with several properties, this function parses it into a list of Properties.
parseProperties :: Object -> Maybe [PT.Property]
parseProperties obj = do
  hits <- parseMaybe (obj .:) "hits"
  hitsHits <- parseMaybe (hits .:) "hits" 
  properties <- mapM parseProperty hitsHits  
  return properties


  
-- This function receives an Object (json) and parses a Bookable if possible.
parseBookable :: Object -> Maybe BT.Bookable
parseBookable object = do
  -- Obligatory Attributes
  _id <- parseStringAttr object "_id"
  _source <- parseMaybe (object .:) "_source" :: Maybe Object
  propId <- parseStringAttr _source "prop_id"
  cCode <- parseStringAttr _source "prop_country_code"
  maxOccu <- parseIntAttr _source "max_occupancy"
  name <- parseStringAttr _source "bkl_name"
  bedNum <- parseIntAttr _source "bed_num"
  status <- parseBoolAttr _source "listed"
  -- Optional fields
  let roomSize = parseOptionalStringAttr _source "room_size"
      bedType = parseOptionalStringAttr _source "bed_type"
      esDesc = parseOptionalStringAttr _source "esp_bkl_desc"
      enDesc = parseOptionalStringAttr _source "eng_bkl_desc"
      facs = parseArrString _source "prop_facilities"
      amens = parseArrString _source "amenities" 
      roomIds = parseArrString _source "room_ids"
  let bklSpecs = BT.BookableSpecs name roomSize bedType bedNum amens    
  return $ BT.Bookable _id
                       (CC.fromText cCode)
                       (if status == True then  BT.Listed else BT.Unlisted)
                       (BT.BasicBookableData propId bklSpecs esDesc enDesc maxOccu (S.fromList roomIds))
                       (getPricings _source)
  where
    parsePricing :: Value -> Maybe BT.Pricing
    parsePricing (Object obj) = do
      let conds = parseArrString obj "conditions"
      priId <- parseStringAttr obj "pri_id"
      occu <- parseIntAttr obj "occupancy"
      price <- parseIntAttr obj "night_price"
      disc <- parseIntAttr obj "discount"
      return $ BT.Pricing priId (BT.PricingData occu conds price disc)
    parsePricing _ = Nothing
    getPricings object = case parseMaybe (object .:) "pricings" of
      Nothing -> S.empty
      Just (Array val) ->
        case mapM parsePricing val of
          Nothing -> S.empty
          Just vec -> S.fromList $ V.toList vec 
      _ -> S.empty




-- Given an Object (json) with several bookables, this function parses it into a list of Bookables.
parseBookables :: Object -> Maybe [BT.Bookable]
parseBookables obj = do
  hits <- parseMaybe (obj .:) "hits"
  hitsHits <- parseMaybe (hits .:) "hits" 
  bookables <- mapM parseBookable hitsHits  
  return bookables




-- This function receives an Object (json) and parses an Article if possible.      
parseArticle :: Object ->  Maybe AT.Article
parseArticle object = do
  -- Obligatory Attributes
  _id <- parseStringAttr object "_id"
  _type <- parseStringAttr object "_type"
  _source <- parseMaybe (object .:) "_source" :: Maybe Object
  let isSpanish = if _type == "esp_article" then True else False
      format = if isSpanish then "format" else "format_eng"
      author = if isSpanish then "author" else "author_eng"
      pubDate = if isSpanish then "pub_date" else "pub_date_eng" 
      title = if isSpanish then "title" else "title_eng"
      abstract = if isSpanish then "abstract" else "abstract_eng"
      sectionTitles = if isSpanish then "section_titles" else "section_titles_eng"
      sections = if isSpanish then "sections" else "sections_eng"
      abstractImageUrl = if isSpanish then "abstract_image_url" else "abstract_image_url_eng"
      mainImageUrl = if isSpanish then "main_image_url" else "main_image_url_eng" 
      thumbnailUrl = if isSpanish then "thumbnail_url" else "thumbnail_url_eng"
      imageUrls = if isSpanish then "image_urls" else "image_urls_eng"
      videoHeaders = if isSpanish then "video_headers" else "video_headers_eng" 
      videoUrls = if isSpanish then "video_urls" else "video_urls_eng"
      country = if isSpanish then "country" else "country_eng"
      region = if isSpanish then "region" else "region_eng"
      city = if isSpanish then "city" else "city_eng"
      propertyName = if isSpanish then "property_name" else "property_name_eng"
  format' <- parseStringAttr _source format
  author' <- parseStringAttr _source author
  pubDate' <- parseStringAttr _source pubDate
  title' <- parseStringAttr _source title
  abstract' <- parseStringAttr _source abstract
  abstractImageUrl' <- parseStringAttr _source abstractImageUrl
  mainImageUrl' <- parseStringAttr _source mainImageUrl
  thumbnailUrl' <- parseStringAttr _source thumbnailUrl
  country' <- parseStringAttr _source country
  region' <- parseStringAttr _source region
  city' <- parseStringAttr _source city
  -- Optional Attributes
  let sectionTitles' = parseArrString _source sectionTitles
      sections' = parseArrString _source sections
      imageUrls' = parseArrString _source imageUrls
      videoHeaders' = parseArrString _source videoHeaders
      videoUrls' = parseArrString _source videoUrls
      propertyName' = parseOptionalStringAttr _source propertyName
  return $ AT.Article _id
                      _type
                      format'
                      author'
                      pubDate'
                      title'
                      abstract'
                      sectionTitles'
                      sections'
                      abstractImageUrl'
                      mainImageUrl'
                      thumbnailUrl'
                      imageUrls'
                      videoHeaders'
                      videoUrls'
                      (AT.Location country' region' city')
                      propertyName'
   

-- This function receives an Object (json) and parses an Article if possible.      
parseAbstract :: Object ->  Maybe AT.Abstract
parseAbstract object = do
  -- Obligatory Attributes
  _id <- parseStringAttr object "_id"
  _type <- parseStringAttr object "_type"
  _source <- parseMaybe (object .:) "_source" :: Maybe Object
  let isSpanish = if _type == "esp_article" then True else False
      format = if isSpanish then "format" else "format_eng"
      author = if isSpanish then "author" else "author_eng"
      pubDate = if isSpanish then "pub_date" else "pub_date_eng" 
      title = if isSpanish then "title" else "title_eng"
      abstract = if isSpanish then "abstract" else "abstract_eng"
      abstractImageUrl = if isSpanish then "abstract_image_url" else "abstract_image_url_eng" 
      thumbnailUrl = if isSpanish then "thumbnail_url" else "thumbnail_url_eng"
  format' <- parseStringAttr _source format
  author' <- parseStringAttr _source author
  pubDate' <- parseStringAttr _source pubDate
  title' <- parseStringAttr _source title
  abstract' <- parseStringAttr _source abstract
  abstractImageUrl' <- parseStringAttr _source abstractImageUrl
  thumbnailUrl' <- parseStringAttr _source thumbnailUrl
  return $ AT.Abstract _id
                       _type
                       format'
                       author'
                       pubDate'
                       title'
                       abstract'
                       abstractImageUrl'
                       thumbnailUrl'
                      

-- Given an Object (json) with several abstracts, this function parses it into a list of Abtracts.
parseAbstracts :: Object -> Maybe [AT.Abstract]
parseAbstracts obj = do
  hits <- parseMaybe (obj .:) "hits"
  hitsHits <- parseMaybe (hits .:) "hits" 
  abstracts <- mapM parseAbstract hitsHits  
  return abstracts


