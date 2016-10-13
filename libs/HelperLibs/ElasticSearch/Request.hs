module HelperLibs.ElasticSearch.Request where

import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Simple as SHTTP


-- This function gathers the most general values needed to build an HTTP request.

partialRequest :: String
               -> Int
               -> String
               -> String
               -> SHTTP.Request
               -> SHTTP.Request
partialRequest method port host path =
       (SHTTP.setRequestMethod $ C8.pack method)
    .  (SHTTP.setRequestPort port)
    .  (SHTTP.setRequestHost $ C8.pack host)
    .  (SHTTP.setRequestPath $ C8.pack path)
