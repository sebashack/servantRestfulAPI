{-# LANGUAGE OverloadedStrings     #-}


module Schemas.ElasticSearch.ContentAdminMappings (
         espArticleMapping,
         engArticleMapping
       ) where

import qualified Data.Text as T
import HelperLibs.ElasticSearch.BodyBuilder


-- Note: the cCode filed is only to filter by country. The country field allows us to make matches without filtering.

-- espArticle Mapping

format = objStr ("format", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
author = objStr ("author", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
pubDate = objStr ("pub_date", [strVal ("type", "date"),
                               strVal ("format", "strict_date"),
                               strVal ("index", "not_analyzed")])
title = objStr ("title", [strVal ("type", "string"), strVal ("analyzer", "esp_analyzer")])
abstract = objStr ("abstract", [strVal ("type", "string"), strVal ("analyzer", "esp_analyzer")])
sectionTitles = objStr ("section_titles", [strVal ("type", "string"), strVal ("analyzer", "esp_analyzer")])
sections = objStr ("sections", [strVal ("type", "string"), strVal ("analyzer", "esp_analyzer")])
abstractImageUrl = objStr ("abstract_image_url", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
mainImageUrl = objStr ("main_image_url", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
thumbnailUrl = objStr ("thumbnail_url", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
imageUrls = objStr ("image_urls", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
videoHeaders = objStr ("video_headers", [strVal ("type", "string"), strVal ("analyzer", "esp_analyzer")])
videoUrls = objStr ("video_urls", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
country = objStr ("country", [strVal ("type", "string"), strVal ("analyzer", "esp_analyzer")])
cCode = objStr ("country_code", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
region = objStr ("region", [strVal ("type", "string"), strVal ("analyzer", "esp_analyzer")])
city = objStr ("city", [strVal ("type", "string"), strVal ("analyzer", "esp_analyzer")])
propertyName = objStr ("property_name", [strVal ("type", "string"), strVal ("index", "not_analyzed")])

espArticleMapping' = objStr("properties", [format, 
                                           author, 
                                           pubDate, 
                                           title, 
                                           abstract, 
                                           sectionTitles, 
                                           sections, 
                                           abstractImageUrl,
                                           mainImageUrl,
                                           thumbnailUrl,
                                           imageUrls,
                                           videoHeaders, 
                                           videoUrls,
                                           country,
                                           cCode,
                                           region, 
                                           city, 
                                           propertyName])

espArticleMapping :: (T.Text, T.Text)
espArticleMapping = ("esp_article", espArticleMapping')

-- engArticle Mapping


formatEng = objStr ("format_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
authorEng = objStr ("author_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
pubDateEng = objStr ("pub_date_eng", [strVal ("type", "date"),
                                      strVal ("format", "strict_date"),
                                      strVal ("index", "not_analyzed")])
titleEng = objStr ("title_eng", [strVal ("type", "string"), strVal ("analyzer", "eng_analyzer")])
abstractEng = objStr ("abstract_eng", [strVal ("type", "string"), strVal ("analyzer", "eng_analyzer")])
sectionTitlesEng = objStr ("section_titles_eng", [strVal ("type", "string"), strVal ("analyzer", "eng_analyzer")])
sectionsEng = objStr ("sections_eng", [strVal ("type", "string"), strVal ("analyzer", "eng_analyzer")])
abstractImageUrlEng = objStr ("abstract_image_url_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
mainImageUrlEng = objStr ("main_image_url_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
thumbnailUrlEng = objStr ("thumbnail_url_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
imageUrlsEng = objStr ("image_urls_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
videoHeadersEng = objStr ("video_headers_eng", [strVal ("type", "string"), strVal ("analyzer", "eng_analyzer")])
videoUrlsEng = objStr ("video_urls_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
countryEng = objStr ("country_eng", [strVal ("type", "string"), strVal ("analyzer", "eng_analyzer")])
cCodeEng = objStr ("country_code_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
regionEng = objStr ("region_eng", [strVal ("type", "string"), strVal ("analyzer", "eng_analyzer")])
cityEng = objStr ("city_eng", [strVal ("type", "string"), strVal ("analyzer", "eng_analyzer")])
propertyNameEng = objStr ("property_name_eng", [strVal ("type", "string"), strVal ("index", "not_analyzed")])


engArticleMapping' = objStr("properties", [formatEng, 
                                           authorEng, 
                                           pubDateEng, 
                                           titleEng, 
                                           abstractEng, 
                                           sectionTitlesEng, 
                                           sectionsEng, 
                                           abstractImageUrlEng,
                                           mainImageUrlEng,
                                           thumbnailUrlEng,
                                           imageUrlsEng,
                                           videoHeadersEng, 
                                           videoUrlsEng,
                                           countryEng,
                                           cCodeEng,
                                           regionEng, 
                                           cityEng, 
                                           propertyNameEng])
                                           
engArticleMapping :: (T.Text, T.Text)
engArticleMapping = ("eng_article", engArticleMapping')
