{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.Interpreters.ContentAdmin where

import Servant


-- Set a numeric default value.
setBound defVal val = if val > 0 then val else defVal

-- Custom servant errors
_articleNotFound = err404 { errBody = "Article Not Found" }
_absNotFound = err404 { errBody = "Abstracts Not Found" }
_parseArtErr = err500 { errBody = "Error While Parsing Article ES Response" }
_parseAbsErr = err500 { errBody = "Error While Parsing Abstract ES Response" }
_unknown = err500 { errBody = "Unkown Error" }




