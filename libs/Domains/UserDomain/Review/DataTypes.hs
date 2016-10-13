{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.UserDomain.Review.DataTypes where

import qualified Data.Time as TM
import qualified Data.Text as T
import GHC.Generics  
import Data.Aeson
import Data.Aeson.Types


-- Every Review belongs to one reservation and one reservation has only one review.

data ReviewData = ReviewData {
  content :: T.Text,
  score :: Int -- An Int from 1 to 10.
  } deriving (Show, Generic, Eq)

instance FromJSON ReviewData
instance ToJSON ReviewData



data Review = Review {
  reservId :: Integer,
  reviewDate :: TM.UTCTime,
  review :: ReviewData
  } deriving (Show, Generic)

instance FromJSON Review
instance ToJSON Review

-- Two reviews are equal if their reservIds are equal.
instance Eq Review where
  (Review id1 _ _) == (Review id2 _ _) =
    id1 == id2






