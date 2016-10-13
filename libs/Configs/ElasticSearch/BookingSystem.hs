{-# LANGUAGE OverloadedStrings     #-}

module Configs.ElasticSearch.BookingSystem
       (
         esBookingDevConfig,
         esBookingProdConfig,
         esBookingTestConfig
       ) where

import Configs.ConfigTypes
import HelperLibs.ElasticSearch.BodyBuilder

-- ANALYZER SETTINGS -- 

analyzerType = strVal("type","custom")

{-
Settings for analyzing tags:
The following analyzer is intended to be used with tags of facilities, ammenities and rules.
Also with a property's name and type.
-}
tagTokenizer = strVal ("tokenizer","standard")
tagFilter = arrVal ("filter", ["lowercase"])

tagAnalyzer = objStr ("tagAnalyzer", [analyzerType, tagTokenizer, tagFilter])


{-
Settings for analyzing a location:
The following analyzer is intended to be used with the country, region and city of the
property.
-}

locTokenizer = strVal ("tokenizer","standard")
locFilter = arrVal ("filter", ["lowercase", "asciifolding"])

locAnalyzer = objStr ("locAnalyzer", [analyzerType, locTokenizer, locFilter])


-- Analyzers

analyzers = objStr ("analyzer", [tagAnalyzer, locAnalyzer]) 


-- END OF ANALYZER SETTINGS -- 


-- ElasticSearch Development Settings --

esBookingDevConfig = ConfigES "localhost"
                              9200
                              "destinos_devel"
                              5
                              1
                              analyzers
                              Nothing
                         

-- TODO: define an index with production settings.


esBookingProdConfig = undefined

esBookingTestConfig = undefined








