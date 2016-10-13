{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.BookingDomain.Bookable.DataTypes where

import qualified Data.Time as TM
import qualified Data.Text as T
import qualified Data.Set as S
import GHC.Generics  
import Data.Aeson
import Data.Aeson.Types
import Data.CountryCodes


data BookableState = Listed | Unlisted
  deriving (Eq, Show, Generic)


instance FromJSON BookableState
instance ToJSON BookableState

{-
For now, we are using a Integer type to represent a price. This is ok in this stage where
we are not dealing with electronic-transactions. A review of money representation
will have to be done in the future when dealing with electronic-transactions.
Also note that COP fits perfectly in an Integer schema, which is ok for our regional beginning.

Implicitly we are forcing a currency per property. Thus if the property is in Venezuela, the currency for all
the pricings of its bookables will be VEF. This won't have to be an explicit field in the Property aggregate. A property
admin will just have to store a price in the local currency of the property country, then the client using the API, when using it
for searching for bookables in Venezuela, will be aware that prices for those bookables are in VEF and will  perform appropriate
convertions (with a valid exchange rate) if, say, it is filtering range prices in USD. In conclusion, every property has an
implicit currency which is equivalent to the currency of the property's country and this allows us to store local prices for
each propery's bookables. This idea will make it easier to extend our application to other countries.

Right now, as we'll start bussiness in a regional context, all properties will be interpreted as having a COP currency.
-}


data PricingData = PricingData {
  occupancy :: Int,
  conditions :: [T.Text],
  nightPrice :: Integer, -- Interpreted according to the currency of the country of the Property.
  roomDiscount :: Int -- An Int between 0 and 100
} deriving (Eq, Show, Generic)
             
instance FromJSON PricingData
instance ToJSON PricingData



data Pricing = Pricing {
  priId :: T.Text, 
  priData :: PricingData
} deriving (Show, Generic)
             
             
-- Two pricings are equal if their ids are equal.
instance Eq Pricing where
  (Pricing id1 _) == (Pricing id2 _) = id1 == id2 
  
-- Pricings are compared by id. 
instance Ord Pricing where
  compare (Pricing id1 _) (Pricing id2 _) = compare id1 id2
  
  
instance FromJSON Pricing
instance ToJSON Pricing


data BookableSpecs = BookableSpecs {
   name :: T.Text,
   roomSize :: Maybe T.Text,
   bedType :: Maybe T.Text,
   bedNum ::Int,
   amenities :: [T.Text]
   } deriving (Eq, Show, Generic)

instance FromJSON BookableSpecs
instance ToJSON BookableSpecs


-- Note that the room ids are a Set cause they cannot contain repeated elements.
data BasicBookableData = BasicBookableData {
  propId :: T.Text, 
  bklSpecs :: BookableSpecs,
  esDesc :: Maybe T.Text,
  enDesc :: Maybe T.Text,
  maxOccu :: Int,  -- specific to a bookable.
  roomIds :: S.Set T.Text -- The ids which identify every reservable unit in the property
  } deriving (Eq, Show, Generic)


instance FromJSON BasicBookableData
instance ToJSON BasicBookableData

-- Note that we are using a set of Prcings so that we avoid duplicate results.
data Bookable = Bookable {
  bklId :: T.Text,   
  countryCode :: CountryCode, -- Important to interpret pricings' currencies.
  status :: BookableState,
  basicData :: BasicBookableData,
  pricings :: S.Set Pricing
} deriving (Show, Generic)


instance FromJSON Bookable
instance ToJSON Bookable


-- Two bookables are equal if their ids are equal.
instance Eq Bookable where
  (Bookable id1 _ _ _ _) == (Bookable id2 _ _ _ _) =
    id1 == id2


data CheckInOut = CheckInOut {
  checkIn ::  TM.UTCTime,
  checkOut :: TM.UTCTime 
  } deriving (Eq, Show, Generic)

instance FromJSON CheckInOut
instance ToJSON CheckInOut


data BasicSearch = BasicSearch {
  country :: CountryCode,
  guestNum :: Int,
  destination :: Maybe T.Text, -- either a region name or a city name.
  checkInOut :: CheckInOut,
  numRooms :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON BasicSearch
instance ToJSON BasicSearch

data PriceRange = PriceRange {
  lowerPrice :: Integer,
  upperPrice :: Integer
  } deriving (Eq, Show, Generic)

instance FromJSON PriceRange
instance ToJSON PriceRange


data AdvancedSearch = AdvancedSearch {
  basicSearch :: BasicSearch,
  priceRange :: Maybe PriceRange,
  discount :: Maybe Int, -- An int between 0 and 100
  propType :: Maybe T.Text,
  propFacilities :: [T.Text],
  bklAmenities :: [T.Text],
  beds :: Maybe Int
  } deriving (Eq, Show, Generic)

instance FromJSON AdvancedSearch
instance ToJSON AdvancedSearch


data SearchResult = SearchResult {
  bookable :: Bookable,
  availableUnits :: Int,
  meanScore :: Float,
  propertyName :: T.Text,
  propertyType :: T.Text,
  mainPropImgId :: Maybe T.Text 
  } deriving (Eq, Show, Generic)

instance FromJSON SearchResult
instance ToJSON SearchResult


data SearchResultWithImgIds = SearchResultWithImgIds {
  searchResult :: SearchResult,
  bklImgIds :: [T.Text]
  } deriving (Eq, Show, Generic)

instance FromJSON SearchResultWithImgIds
instance ToJSON SearchResultWithImgIds


data Report = Report {
  totalUnits :: S.Set T.Text,
  reservedUnits :: S.Set T.Text,
  remainingUnits :: S.Set T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON Report
instance ToJSON Report



data Period = Period {
  from :: TM.UTCTime,
  to :: TM.UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON Period
instance ToJSON Period


data PeriodReport = PeriodReport {
  reportBkl :: Bookable,
  periodReport :: Report
  } deriving (Eq, Show, Generic)

instance FromJSON PeriodReport
instance ToJSON PeriodReport



data EconomicReport = EconomicReport {
  period :: Period,
  reservedRooms :: Integer,
  cancelledRooms :: Integer,
  reservationIncome :: Integer,
  cancellationLoss :: Integer 
  } deriving (Eq, Show, Generic)

instance FromJSON EconomicReport
instance ToJSON EconomicReport 


data UserCredentials = UserCredentials {
  profNameOrEmail :: T.Text,
  password :: T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserCredentials
instance ToJSON UserCredentials











