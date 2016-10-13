{-# LANGUAGE OverloadedStrings     #-}

module HelperLibs.ElasticSearch.IndexScripts where

import Configs.ElasticSearch.BookingSystem
import Configs.ElasticSearch.ContentAdmin
import Configs.ConfigTypes
import Schemas.ElasticSearch.BookingMappings
import Schemas.ElasticSearch.ContentAdminMappings
import HelperLibs.ElasticSearch.BodyBuilder
import HelperLibs.ElasticSearch.Request
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import HelperLibs.ElasticSearch.ResponseParser
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Simple as SHTTP
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T


-- Given a ConfigES create an index.
createIndex :: ConfigES -> IO String
createIndex config = do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 $ braces settings) :: Either String Value
  case mQueryBody of
    Left err -> return err
    Right queryBody -> do
      let makeReq =   (partialRequest "PUT" port host $ "/" ++ name ++  "/")
                    . (SHTTP.setRequestBodyJSON queryBody)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        200 -> return $ "Index " ++ "'" ++ name ++ "'" ++ "successfully created."
        _ -> return $ "Something went wrong when creating index."
  where
    (ConfigES host port name shards repls analz mFilter) = config
    settings = case mFilter of
      Nothing -> objStr ("settings", [intVal ("number_of_shards", shards),
                                      intVal ("number_of_replicas", repls),
                                      objStr ("index", [objStr ("analysis", [analz])])])
      (Just aFilter) -> objStr ("settings", [intVal ("number_of_shards", shards),
                                             intVal ("number_of_replicas", repls),
                                             objStr ("index", [objStr ("analysis", [analz, aFilter])])])



-- Given an index name, a host and a port, delete that index.
deleteIndex :: String -> String -> Int -> IO String
deleteIndex name host port = do
  let makeReq = partialRequest "DELETE" port host ("/" ++ name ++ "/")
  response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
  logResponse response
  case SHTTP.getResponseStatusCode response of
    200 -> return $ "Index " ++ "'" ++ name ++ "'" ++ "successfully deleted."
    _ -> return $ "Something went wrong when deleting index."



-- Given an index name, a host and a port, get information about that index.
getIndex :: String
         -> String
         -> Int
         -> IO (SHTTP.Response Value)
getIndex name host port = do
  let makeReq = partialRequest "GET" port host ("/" ++ name ++"/")
  response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
  return response




-- Given a (typeName, mapping), an index name, a host and a port, this function
-- puts a new mapping in the given index.

putMapping :: (T.Text, T.Text)
           -> String
           -> String
           -> Int
           -> IO String
putMapping (typeName, mapping) indexName host port = do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 $ braces mapping) :: Either String Value
  case mQueryBody of
    Left err -> return err
    Right queryBody -> do
      let makeReq =   (partialRequest "PUT"
                                      port
                                      host
                                      $ "/" ++ indexName ++ "/" ++ "_mapping/" ++ (T.unpack typeName)) 
                    . (SHTTP.setRequestBodyJSON queryBody)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        200 -> return $ "Mapping was successfully created."
        _ -> return $ "Something went wrong when creating mapping."
 


-- Given a type name, an index name, a host and a port, get information about that mapping.
getMapping :: String
           -> String
           -> String
           -> Int
           -> IO ()
getMapping indexName typeName host port = do
  let makeReq = partialRequest "GET" port host ("/" ++ indexName ++ "/_mapping/" ++ typeName)
  response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
  logResponse response
  


-- Given an index name, an analyzer name and a text, this function gives us information about
-- the way strings are being analyzed.

analyzeText :: String
            -> String
            -> String
            -> String
            -> Int
            -> IO String
analyzeText indexName anName text host port = do
 let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 $ braces body) :: Either String Value
 case mQueryBody of
   Left err -> return err
   Right queryBody -> do
     let makeReq =   (partialRequest "GET" port host $ "/" ++ indexName  ++ "/_analyze")
                   . (SHTTP.setRequestBodyJSON queryBody)
     response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
     logResponse response
     case SHTTP.getResponseStatusCode response of
       200 -> return $ "Text was analyzed successfully."
       _ -> return $ "Something went wrong when analyzing text."
  where
    body = strVal ("analyzer", (T.pack anName)) <> ", " <> strVal ("text", (T.pack text))



