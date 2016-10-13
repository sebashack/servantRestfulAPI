{-# LANGUAGE OverloadedStrings     #-}


module HelperLibs.ElasticSearch.BookableRepo where

import Data.Aeson
import Data.Aeson.Types
import HelperLibs.ElasticSearch.BodyBuilder
import HelperLibs.ElasticSearch.Request
import HelperLibs.ElasticSearch.ResponseParser
import Control.Monad (replicateM)
import System.Random
import Data.Maybe
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Network.HTTP.Simple as SHTTP
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding as TE



-- Set a null json value for a Nothing
setValOrNull :: T.Text -> Maybe T.Text -> T.Text
setValOrNull key Nothing = nullVal key
setValOrNull key (Just val) = strVal (key, val)


-- General purpose update function.
updateBookable :: String
               -> Int
               -> String
               -> T.Text
               -> T.Text
               -> IO (Either String Value)
updateBookable host port iName bklId updates = do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 $ braces updates) :: Either String Value
  case mQueryBody of
    Left err -> return $ Left err
    Right body -> do
      let path = "/" ++ iName ++  "/api_bookable/" ++ (T.unpack bklId) ++ "/_update"
          makeReq =  (partialRequest "POST" port host path)
                   . (SHTTP.setRequestBodyJSON body)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        200 -> do
          let resBody = SHTTP.getResponseBody response
          return $ Right (resBody :: Value)
        _  -> return $ Left ("Bookable " ++ (T.unpack bklId) ++ " couldn't be updated")   





-- General query request for in Properties.

bookableQuery :: String
              -> Int
              -> String
              -> T.Text
              ->  IO (Either String Value)
bookableQuery host port iName query = do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 query) :: Either String Value
  case mQueryBody of
    Left err -> return $ Left err
    Right body -> do
      let makeReq =  (partialRequest "GET" port host $ "/" ++ iName ++  "/api_bookable/_search")
                   . (SHTTP.setRequestBodyJSON body)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      print query
      logResponse response
      case SHTTP.getResponseStatusCode response of
        200 -> do
          let resBody = SHTTP.getResponseBody response
          return $ Right (resBody :: Value)
        _  -> return $ Left "ElasticSearch Error"   




-- transform a the data of a pricing into a JSON string representation.

pricingDataToJSONstring :: (T.Text, Int, [T.Text], Integer, Int) -> T.Text
pricingDataToJSONstring (priId, occu, conds, nightPri, disc) =
  objVal [strVal ("pri_id", priId),
          intVal ("occupancy", occu),
          arrVal ("conditions", conds),
          intVal ("night_price", nightPri),
          intVal ("discount", disc)]



-- Given a bookableId, a UTC date and maybe a Name, this function generates a unique random
-- id code for an image.

genBklImgId :: T.Text -> IO ByteString
genBklImgId bklId = do
  (TM.UTCTime date time) <- TM.getCurrentTime
  let (year, monthDay) = splitAt 4 (show date)
      (month, day') = splitAt 2 (tail monthDay)
      day = tail day'
      hour = show $ round (time * 1000000)
  return $ TE.encodeUtf8 $ T.pack $ (T.unpack bklId) ++ (drop 1 year) ++ month ++ day ++ hour





-- Build a query string from a list of strings.
makeTextQuery :: [T.Text] -> T.Text
makeTextQuery [] = ""
makeTextQuery list = go list ""
  where
    go [] result = result
    go [t] result = result <> t 
    go (t:ts) result = go ts (result <> t <> " ")


-- make a clause (should, must, must_not) for an elastic search compund query
clauseQuery :: T.Text -> [Maybe T.Text] -> T.Text
clauseQuery clause [] = arrObj (clause , [])
clauseQuery clause mFilters = arrObj (clause , filters)
  where
    filters = catMaybes mFilters
    



