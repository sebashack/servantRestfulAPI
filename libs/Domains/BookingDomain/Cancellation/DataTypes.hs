{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.BookingDomain.Cancellation.DataTypes where

import qualified Data.Time as TM
import qualified Data.Text as T
import qualified Data.Set as S
import GHC.Generics  
import Data.Aeson
import Data.Aeson.Types


data CancellationData = CancellationData {
  reservedPricing :: Integer, -- id of pricing reserved by user.
  numRooms :: Int -- This is handy for cancellations in non-expired Pending Reservations.
  } deriving (Eq, Show, Generic)
             
instance FromJSON CancellationData
instance ToJSON CancellationData


data Cancellation = Cancellation {
  reservId :: Integer,
  cancelled :: TM.UTCTime,
  cancelledRooms :: S.Set T.Text,
  byAdmin :: Bool,  -- If made by a user or by and admin.
  cancData :: CancellationData
  } deriving (Show, Generic)
             
instance FromJSON Cancellation
instance ToJSON Cancellation



instance Eq Cancellation where
  (Cancellation id1 _ _ _ _) == (Cancellation id2 _ _ _ _) =
    id1 == id2

