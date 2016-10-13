{-# LANGUAGE OverloadedStrings     #-}


module Configs.ElasticSearch.ContentAdmin
       (
         esArticlesDevConfig,
         esArticlesProdConfig,
         esArticlesTestConfig
       ) where

import Configs.ConfigTypes
import HelperLibs.ElasticSearch.BodyBuilder

-- ANALYZER SETTINGS --

analyzerType = strVal("type","custom")

-- Filter Settings

espLength = objStr ("esp_length", [strVal ("type","length"),
                                   intVal ("min", 4),
                                   intVal ("max", 20)])
espStop = objStr ("esp_stop", [strVal ("type","stop"),
                               strVal ("stopwords", "_spanish_")])
espStemmer = objStr ("esp_stemmer", [strVal ("type","stemmer"),
                                     strVal ("name", "spanish")])
engLength = objStr ("eng_length", [strVal ("type","length"),
                                   intVal ("min", 3),
                                   intVal ("max", 20)])
engStop = objStr ("eng_stop", [strVal ("type","stop"),
                               strVal ("stopwords", "_english_")])
engStemmer = objStr ("eng_stemmer", [strVal ("type","stemmer"),
                                     strVal ("name", "english")])
possessiveEng = objStr ("possessive_eng", [strVal ("type","stemmer"),
                                           strVal ("name", "possessive_english")])



-- Settings for analyzing articles in Spanish: 

esTokenizer = strVal ("tokenizer","standard")
esFilter = arrVal ("filter", ["lowercase", "esp_length", "esp_stop", "asciifolding", "esp_stemmer"])

esAnalyzer = objStr ("esp_analyzer", [analyzerType, esTokenizer, esFilter])

-- Settings for analyzing articles in English:

enTokenizer = strVal ("tokenizer","standard")
enFilter = arrVal ("filter", ["lowercase", "possessive_eng", "eng_length", "eng_stop", "eng_stemmer"])

enAnalyzer = objStr ("eng_analyzer", [analyzerType, enTokenizer, enFilter])


-- Analyzers

analyzers = objStr ("analyzer", [esAnalyzer, enAnalyzer]) 
analyzerFilter = objStr ("filter", [espLength,
                                    espStop,
                                    espStemmer,
                                    engLength,
                                    engStop,
                                    engStemmer,
                                    possessiveEng])



-- ElasticSearch Development Settings --

esArticlesDevConfig = ConfigES "localhost"
                               9200
                               "articles_devel"
                               5
                               1
                               analyzers
                               (Just analyzerFilter)




esArticlesProdConfig = undefined
esArticlesTestConfig = undefined






