module HelperLibs.ElasticSearch.PropertyRepo where

import Data.Aeson
import Data.Aeson.Types
import HelperLibs.ElasticSearch.BodyBuilder
import HelperLibs.ElasticSearch.Request
import HelperLibs.ElasticSearch.ResponseParser
import Control.Monad (replicateM)
import System.Random
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Network.HTTP.Simple as SHTTP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding as TE


-- General purpose update function.
updateProperty :: String
               -> Int
               -> String
               -> T.Text
               -> T.Text
               -> IO (Either String Value)
updateProperty host port iName propId updates = do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 $ braces updates) :: Either String Value
  case mQueryBody of
    Left err -> return $ Left err
    Right body -> do
      let path = "/" ++ iName ++  "/api_property/" ++ (T.unpack propId) ++ "/_update"
          makeReq =  (partialRequest "POST" port host path)
                   . (SHTTP.setRequestBodyJSON body)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        200 -> do
          let resBody = SHTTP.getResponseBody response
          return $ Right (resBody :: Value)
        _  -> return $ Left ("Property " ++ (T.unpack propId) ++ " couldn't be updated")   


-- General query request for in Properties.

propertyQuery :: String
              -> Int
              -> String
              -> T.Text
              ->  IO (Either String Value)
propertyQuery host port iName query = do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 query) :: Either String Value
  case mQueryBody of
    Left err -> return $ Left err
    Right body -> do
      let makeReq =  (partialRequest "GET" port host $ "/" ++ iName ++  "/api_property/_search")
                   . (SHTTP.setRequestBodyJSON body)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        200 -> do
          let resBody = SHTTP.getResponseBody response
          return $ Right (resBody :: Value)
        _  -> return $ Left "ElasticSearch Error"   



-- Given a propertyId, a UTC date and maybe a Name, this function generates a unique random
-- id code for an image.

genPropImgId :: T.Text -> IO ByteString
genPropImgId propId = do
  (TM.UTCTime date time) <- TM.getCurrentTime
  let (year, monthDay) = splitAt 4 (show date)
      (month, day') = splitAt 2 (tail monthDay)
      day = tail day'
      hour = show $ round (time * 1000000)
  return $ TE.encodeUtf8 $ T.pack $ (T.unpack propId) ++ (drop 1 year) ++ month ++ day ++ hour
  


-- Set a value or a default value

setBound :: Maybe Int -> Int -> Int
setBound mVal defVal = case mVal of
  Nothing -> defVal
  Just val -> if val > 0 then val else defVal
                              








