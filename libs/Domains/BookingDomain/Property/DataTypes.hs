{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.BookingDomain.Property.DataTypes where

import Data.Monoid 
import GHC.Generics  
import Data.Aeson
import Data.Aeson.Types
import Data.CountryCodes
import Control.Monad (mzero)
import qualified Data.Time as TM
import qualified Data.Text as T


data Location = Location {
  countryCode :: CountryCode,
  region :: T.Text,
  city :: T.Text,
  address :: T.Text,
  zipCode :: Maybe T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON Location
instance ToJSON Location



-- This data type is defined in order to make the semantics of updating a Location clarer.
-- That is, the country of a Property, once it is created, cannot be updated due to currency and
-- local time issues.

data Region = Region T.Text T.Text T.Text (Maybe T.Text) deriving (Show, Eq)


instance FromJSON Region where
  parseJSON (Object v) = Region <$>
                         v .: "region" <*>
                         v .: "city" <*>
                         v .: "address" <*>
                         v .: "zipCode"
  parseJSON _ = mzero



instance ToJSON Region where
  toJSON (Region reg ci adds zCode) = 
    object ["region" .= reg, "city" .= ci, "address" .= adds, "zipCode" .= zCode]
  toEncoding (Region reg ci adds zCode) =
    pairs ("region" .= reg <> "city" .= ci <> "address" .= adds <> "zipCode" .= zCode)




-- A name, a property type and a location are the basic information to create a property.

data BasicPropData = BasicPropData {
  name :: T.Text,
  propType :: T.Text,
  location :: Location
  } deriving (Eq, Show, Generic)


instance FromJSON BasicPropData
instance ToJSON BasicPropData

 
data Property = Property {
  propId :: T.Text,
  propData :: BasicPropData,
  regDate :: TM.UTCTime,
  esDesc :: Maybe T.Text,
  enDesc :: Maybe T.Text,
  facilities :: [T.Text],
  rules :: [T.Text],
  contactPhones :: [T.Text],
  mainImgId :: Maybe T.Text
  } deriving (Show, Generic)


instance FromJSON Property
instance ToJSON Property



-- Two properties are equal if they registration codes are equal.
instance Eq Property where
  (Property regCode1 _ _ _ _ _ _ _ _) == (Property regCode2 _ _ _ _ _ _ _ _)
    = regCode1 == regCode2



data PropertyWithImgIds = PropertyWithImgIds {
  property :: Property,
  propImgIds :: [T.Text]
  } deriving (Eq, Show, Generic)


instance FromJSON PropertyWithImgIds
instance ToJSON PropertyWithImgIds




data Period = Period {
    from :: TM.UTCTime,
    to :: TM.UTCTime
} deriving (Eq, Show, Generic)

instance FromJSON Period
instance ToJSON Period















