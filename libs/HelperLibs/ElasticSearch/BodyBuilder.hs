{-# LANGUAGE OverloadedStrings     #-}

module HelperLibs.ElasticSearch.BodyBuilder where

import Data.Monoid
import qualified Data.Text as T


quoteString :: T.Text -> T.Text
quoteString str = "\"" <> str <> "\""

keyString :: T.Text -> T.Text
keyString str = (quoteString str) <> ":"

{-
The following functions are helpers that let you build JSON-like strings easily. These functions do not
enforce any particular type of string in their input. Thus, the correctness of the JSON format depends
on the programmer providing the correct structure of a JSON object.
-}

nullVal :: T.Text -> T.Text
nullVal key = keyString key <> "null"

-- key: string value
strVal :: (T.Text, T.Text) -> T.Text
strVal (key, val) = keyString key <> quoteString val

-- key: double value
doubleVal :: (T.Text, Double) -> T.Text
doubleVal (key, val) = keyString key <> (T.pack $ show val)

-- key: Int value
intVal :: (Integral a, Show a) => (T.Text, a) -> T.Text
intVal (key, val) = keyString key <> (T.pack $ show val)


-- key: Boolean value
boolVal :: (T.Text, Bool) -> T.Text
boolVal (key, val) = keyString key <> boolean 
  where
    boolean = case val of
      True -> "true"
      False -> "false"


-- key: [string] value
arrVal :: (T.Text, [T.Text]) -> T.Text
arrVal (key, []) = keyString key <> "null"
arrVal (key, list) = keyString key <> (T.pack $ show list)


-- key: [obj] value
arrObj :: (T.Text, [T.Text]) -> T.Text
arrObj (key, []) = keyString key <> "[]"
arrObj (key, list) = keyString key <> "[" <> (go list "") <> "]" 
  where go [] result = result
        go [x] result = result <> x 
        go (x:xs) result = go xs (result <> x <> ", ")



-- Object value
objVal :: [T.Text] -> T.Text
objVal [] = "{}"
objVal list = "{" <> (go list "") <> "}" 
  where go [] result = result
        go [x] result = result <> x 
        go (x:xs) result = go xs (result <> x <> ", ")


-- key: object value
objStr :: (T.Text, [T.Text]) -> T.Text
objStr (key, list) = keyString key <> (objVal list)


-- Surround with braces
braces :: T.Text -> T.Text
braces str = "{" <> str <> "}"
