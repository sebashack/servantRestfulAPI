{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.ContentAdminDomain.Article.DataTypes where

import qualified Data.Time as TM
import qualified Data.Text as T
import Control.Monad (mzero)
import Data.Monoid 
import GHC.Generics  
import Data.Aeson
import Data.Aeson.Types
import Data.CountryCodes

{-
The ContentDomain API does not have endpoint to publish Articles. They are stored in ES through
Repository Operations.
-}

data Location = Location {
  country :: T.Text,
  region :: T.Text,
  city :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON Location
instance ToJSON Location


data Article = Article {
  artId :: T.Text,
  artType :: T.Text, -- either esp_article or eng_article
  format :: T.Text,
  author :: T.Text,
  pubDate :: T.Text,
  title :: T.Text,
  abstract :: T.Text,
  sectionTitles :: [T.Text],
  sections :: [T.Text],
  abstractImageUrl :: T.Text,
  mainImageUrl :: T.Text,
  thumbnailUrl :: T.Text,
  imageUrls :: [T.Text],
  videoHeaders :: [T.Text],
  videoUrls :: [T.Text],
  location :: Location,
  propertyName :: Maybe T.Text
  } deriving (Eq, Show, Generic)
             
instance FromJSON Article
instance ToJSON Article

data Abstract = Abstract T.Text T.Text T.Text T.Text T.Text T.Text T.Text T.Text T.Text  deriving (Show, Eq)

instance FromJSON Abstract where
  parseJSON (Object v) = Abstract <$>
                         v .: "artId" <*>
                         v .: "artType" <*>
                         v .: "format" <*>
                         v .: "author" <*>
                         v .: "pubDate" <*>
                         v .: "title" <*>
                         v .: "abstract" <*>
                         v .: "abstractImageUrl" <*>
                         v .: "thumbnailUrl"
  parseJSON _ = mzero



instance ToJSON Abstract where
  toJSON (Abstract a b c d e f g h i) = 
    object ["artId" .= a,
            "artType" .= b,
            "format" .= c, 
            "author" .= d, 
            "pubDate" .= e, 
            "title" .= f, 
            "abstract" .= g, 
            "abstractImageUrl" .= h, 
            "thumbnailUrl" .= i]
  toEncoding (Abstract a b c d e f g h i) =
    pairs ("artId" .= a <>
           "artType" .= b <>
           "format" .= c <> 
           "author" .= d <> 
           "pubDate" .= e <>
           "title" .= f <>
           "abstract" .= g <> 
           "abstractImageUrl" .= h <> 
           "thumbnailUrl" .= i)





data ArticleWithAbstracts = ArticleWithAbstracts {
  article :: Article,
  abstracts :: [Abstract]
  } deriving (Eq, Show, Generic)

instance FromJSON ArticleWithAbstracts
instance ToJSON ArticleWithAbstracts













