{-# LANGUAGE OverloadedStrings     #-}

module HelperLibs.ElasticSearch.ContentAdminRepo where

import Data.Aeson
import Data.Aeson.Types
import HelperLibs.ElasticSearch.BodyBuilder
import HelperLibs.ElasticSearch.Request
import HelperLibs.ElasticSearch.ResponseParser
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Network.HTTP.Simple as SHTTP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding as TE



type ArticleAttrs' = (T.Text, -- format
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


buildArticle :: Bool -> ArticleAttrs' -> T.Text
buildArticle isSpanish artAttrs
  | isSpanish = esDocument
  | otherwise = enDocument 
  where
    (format, author, pubDate, title, abstract, sectionTitles, sections,
     abstractImageUrl, mainImageUrl, thumbnailUrl, imageUrls, videoHeaders,
     videoUrls, country, region, city, propertyName) = artAttrs
    esDocument = objVal [strVal ("format", format),
                         strVal ("author", author),
                         strVal ("pub_date", pubDate),
                         strVal ("title", title),
                         strVal ("abstract", abstract),
                         arrVal ("section_titles", sectionTitles),
                         arrVal ("sections", sections),
                         strVal ("abstract_image_url", abstractImageUrl),
                         strVal ("main_image_url", mainImageUrl),
                         strVal ("thumbnail_url", thumbnailUrl),
                         arrVal ("image_urls", imageUrls),
                         arrVal ("video_headers", videoHeaders),
                         arrVal ("video_urls", videoUrls),
                         strVal ("country", country),
                         strVal ("region", region),
                         strVal ("city", city),
                         strVal ("property_name", propertyName)]
    enDocument = objVal [strVal ("format_eng", format),
                         strVal ("author_eng", author),
                         strVal ("pub_date_eng", pubDate),
                         strVal ("title_eng", title),
                         strVal ("abstract_eng", abstract),
                         arrVal ("section_titles_eng", sectionTitles),
                         arrVal ("sections_eng", sections),
                         strVal ("abstract_image_url_eng", abstractImageUrl),
                         strVal ("main_image_url_eng", mainImageUrl),
                         strVal ("thumbnail_url_eng", thumbnailUrl),
                         arrVal ("image_urls_eng", imageUrls),
                         arrVal ("video_headers_eng", videoHeaders),
                         arrVal ("video_urls_eng", videoUrls),
                         strVal ("country_eng", country),
                         strVal ("region_eng", region),
                         strVal ("city_eng", city),
                         strVal ("property_name_eng", propertyName)]




-- General query request for articles.

articleQuery :: String
             -> Int
             -> String
             -> String
             -> T.Text
             -> IO (Either String Value)
articleQuery host port iName artType query = do
  let mQueryBody = eitherDecode' $ LC8.fromStrict (TE.encodeUtf8 query) :: Either String Value
  case mQueryBody of
    Left err -> return $ Left err
    Right body -> do
      let makeReq =  (partialRequest "GET" port host $ "/" ++ iName ++  "/" ++ artType  ++ "/_search")
                   . (SHTTP.setRequestBodyJSON body)
      response <- SHTTP.httpJSON (makeReq SHTTP.defaultRequest)
      logResponse response
      case SHTTP.getResponseStatusCode response of
        200 -> do
          let resBody = SHTTP.getResponseBody response
          return $ Right (resBody :: Value)
        _  -> return $ Left "ElasticSearch Error"   





artType :: Bool -> String
artType isSpanish 
  | isSpanish = "esp_article"
  | otherwise = "eng_article"


sourceFields :: Bool -> [T.Text]
sourceFields isSpanish
  | isSpanish = ["format", "author", "pub_date", "title", "abstract", "abstract_image_url", "thumbnail_url"]
  | otherwise = ["format_eng", "author_eng", "pub_date_eng", "title_eng", "abstract_eng", "abstract_image_url_eng", "thumbnail_url_eng"]
 
