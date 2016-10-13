{-# LANGUAGE OverloadedStrings     #-}

module Repositories.ContentAdminRepo.Operations where


import Configs.ConfigTypes
import HelperLibs.ElasticSearch.BodyBuilder
import HelperLibs.ElasticSearch.Request
import HelperLibs.ElasticSearch.ResponseParser
import HelperLibs.ElasticSearch.ContentAdminRepo
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

import Data.Monoid

type ArticleAttrs = (T.Text, -- format
                     T.Text, -- author
                     T.Text, -- pub_date
                     T.Text, -- title
                     T.Text, -- abstract
                     [T.Text], -- section_titles
                     [T.Text], -- sections
                     T.Text, -- abstract_image_url
                     T.Text, -- main_image_url
                     T.Text, -- thumbnail_url
                     [T.Text], -- image_urls
                     [T.Text], -- video_headers
                     [T.Text], -- video_urls
                     T.Text, -- country 
                     T.Text, -- region
                     T.Text, -- city
                     T.Text )  -- property_name

  
-- Index an article Spanish
indexArticle :: ConfigES
             -> Bool
             -> ArticleAttrs
             -> ExceptT String IO Value
indexArticle  config isSpanish artAttrs = ExceptT $ do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 document) :: Either String Value
  case mQueryBody of
    Left err -> return $ Left err
    Right body -> do
      let makeReq =  (partialRequest "POST" port host $ "/" ++ iName ++  artType)
                   . (SHTTP.setRequestBodyJSON body)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        201 -> do
          let resBody = SHTTP.getResponseBody response
          return $ Right (resBody :: Value)
        _  -> return $ Left "Article couldn't be indexed"   
  where
    (ConfigES host port iName _ _ _ _) = config
    artType
      | isSpanish = "/esp_article/"
      | otherwise = "/eng_article/"
    document = buildArticle isSpanish artAttrs



-- Query an article by its id
queryArticleById :: ConfigES
                 -> T.Text
                 -> Bool
                 -> ExceptT String IO Value
queryArticleById config artId isSpanish = ExceptT $ do
  let makeReq = partialRequest "GET" port host ("/" ++ iName ++ "/" ++ (artType isSpanish) ++ "/" ++ (T.unpack artId))
  response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
  logResponse response
  case SHTTP.getResponseStatusCode response of
    200 -> do
      let resBody = SHTTP.getResponseBody response
      return $ Right (resBody :: Value)
    _  -> return $ Left ("Aticle with id " ++ (T.unpack artId) ++ " was not found")
  where
    (ConfigES host port iName _ _ _ _) = config




-- Given a query text, search articles which match that text.
queryArticles :: ConfigES
              -> Bool
              -> Integer
              -> Integer
              -> ExceptT String IO Value
queryArticles config isSpanish from size = ExceptT $ articleQuery host port iName artType query
  where
    (ConfigES host port iName _ _ _ _) = config
    artType :: String
    artType
      | isSpanish = "esp_article"
      | otherwise = "eng_article"
    dateField :: T.Text
    dateField 
      | isSpanish = "pub_date"
      | otherwise = "pub_date_eng"
    query = objVal [objStr ("query", [objStr ("match_all", [])]),
                           arrVal ("_source", sourceFields isSpanish),
                           intVal ("from", from),
                           intVal ("size", size)]






-- Given a query text, search articles which match that text.
queryArticlesByText :: ConfigES
                    -> Bool
                    -> T.Text
                    -> Integer
                    -> Integer
                    -> ExceptT String IO Value
queryArticlesByText config isSpanish words from size = 
  ExceptT $ articleQuery host port iName (artType isSpanish) query
  where
    (ConfigES host port iName _ _ _ _) = config
    fields 
      | isSpanish = ["title^3", "abstract^2", "section_titles^2", "sections",
                     "video_headers^2", "country", "region", "city"]
      | otherwise = ["title_eng^3", "abstract_eng^2", "section_titles_eng^2",
                     "sections_eng", "video_headers_eng^2", "country_eng",
                     "region_eng", "city_eng"]
    mMatch = [strVal ("query", words), strVal ("operator", "or"), arrVal ("fields", fields)]  
    query = objVal [objStr ("query", [objStr ("multi_match", mMatch)]),
                           arrVal ("_source", sourceFields isSpanish),
                           intVal ("from", from),
                           intVal ("size", size)]



-- Given a country code and a location string, find articles related with a location.
-- The country names stored in articles should be the complete standarized name in english
-- or spanish according to the article type.
queryArticlesByLoc :: ConfigES
                   -> Bool
                   -> T.Text
                   -> T.Text
                   -> T.Text
                   -> Integer
                   -> Integer
                   -> ExceptT String IO Value
queryArticlesByLoc config isSpanish cCode region city from size = do
  ExceptT $ articleQuery host port iName (artType isSpanish) query
  where
    (ConfigES host port iName _ _ _ _) = config
    region'
      | isSpanish = "region"
      | otherwise = "region_eng"
    city'
      | isSpanish = "city"
      | otherwise = "city_eng"
    cCode'
      | isSpanish = "country_code"
      | otherwise = "country_code_eng"  
    should = arrObj ("should", [braces $ objStr ("match", [strVal (region', region)]),
                                braces $ objStr ("match", [objStr (city', [strVal ("query", city),
                                                                          intVal ("boost", 2) ])]) ]) -- boost the city field      
    filt = arrObj ("must", [braces $ objStr ("term", [strVal (cCode', cCode)])])
    filters = objStr ("filter", [objStr ("bool", [filt])])   
    query =  objVal [objStr ("query", [objStr ("bool", [should,
                                                        filters,
                                                        intVal ("minimum_should_match", 1) ])]),
                     arrVal ("_source", sourceFields isSpanish), 
                     intVal ("from", from),
                     intVal ("size", size)]







