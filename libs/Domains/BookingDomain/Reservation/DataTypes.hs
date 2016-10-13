{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.BookingDomain.Reservation.DataTypes where

import qualified Data.Time as TM
import qualified Data.Text as T
import qualified Data.Set as S
import GHC.Generics  
import Data.Aeson
import Data.Aeson.Types


{-
A reservation can be only in one of four states: Pending, Accepted, Rejected, Absent.

These are the rules of state transition for a reservation made by a registered user:

-- Its state on creation is Pending.

-- Pending reservations do not block the calendar: It is just when an admin assigns rooms in a calendar that
   blocking occurs. Thus, only Accepted reservations block a calendar.

-- A property Admin can change the state of a Prending reservation to Accepted. Once a reservation is Accepted,
   its state cannot be changed to Pending or Rejected.

-- A property Admin can change the state of a Pending reservation to Rejected. Once a reservation is Rejected,
   its state cannot be change to Pending or Accepted.

-- If a Pending reservation has not been Accepted or Rejected by an Admin user in a span of 24 hours
   (1 day after submission date), it is asumed as expired.

-- A reservation with Pending state and which has expired cannot transition to Accepted: It will be assumed as
   Invalid.

-- If a guest has not showed up 24-hours after checkIn (that is, the day after checkIn), a property Admin will be
   allowed to change the state of the reservation to Absent. Once a reservation is in Absent state, it cannot be
   changed to Pending, Accepted nor Rejected. This means that ONLY an Accepted reservation can transition to Absent.
   If the Admin of the property doest not change the state of an Accepted reservation in a interval (checkIn + 24h, checkOut),
   he won't be allowed to change the state of that accepted reservation to Absent.

-- The only state which blocks a calendar is Accepted. Rejected and Absent states will unblock
   calendar dates, so that they can be requested by other users.


For a reservation made by an admin user take the following into account:

-- Its state on creation is Accepted.

-- If a guest has not showed up 24-hours after checkIn (that is, the day after checkIn), a property Admin will be
   allowed to change the state of the reservation to Absent. Once a reservation is in Absent state, it cannot be
   changed to Pending, Accepted nor Rejected. This means that ONLY an Accepted reservation can transition to Absent.
 

-- guestArrived is False in creation. It can only be change to True after checkIn. If that state has been 
   changed to True, it cannot be changed again to False. An Accepted reservation with guestArrived == True cannot be
   changed to Absent. This additional attribute is just to help the admin keep record of the reservations that he can
   change to Absent. In case he does not notify if the guest arrives or not, the transition rules for a Reservation 
   state apply unconditionally. In other words, it is completly the responsibility of the admin to notify if the
   guests arrive, that is, it is up to him to make his like easier.
-}

data ReservState = Pending | Accepted | Rejected | Absent
  deriving (Eq, Show, Generic)

instance FromJSON ReservState
instance ToJSON ReservState


data PricingInfo = PricingInfo {
  priInfoId :: Integer, 
  bklId :: T.Text, -- the Id of the reserved bookable.
  bklName :: T.Text,
  conditions :: S.Set T.Text, 
  occupancy :: Int,
  resCheckIn :: TM.UTCTime,
  resCheckOut :: TM.UTCTime,
  numRooms :: Int,
  nightPrice :: Integer,
  assignedRooms :: S.Set T.Text,  
  discount :: Int  -- An int between 1 and 100
  } deriving (Show, Generic)
             
instance FromJSON PricingInfo
instance ToJSON PricingInfo

instance Eq PricingInfo where
  (PricingInfo id1 _ _ _ _ _ _ _ _ _ _) == (PricingInfo id2 _ _ _ _ _ _ _ _ _ _) =
    id1 == id2




-- In order for a reservation to be created, the total number of reserved units for each bookable must be available.
-- Thus, if there is at least one bookable which has no availability, a reservation cannot be created.

data UserPricing = UserPricing {
  userBkl :: T.Text,          -- Id of bookable 
  userPricing :: T.Text,      -- Id of pricing 
  roomNum :: Int
  } deriving (Show, Generic)

-- Two userPricings are equal if their pricingIds are equal. This is made so in order
-- to avoid duplicates.
instance Eq UserPricing where
  (UserPricing _ id1 _) == (UserPricing _ id2 _) = id1 == id2 
  
  
-- UserPricings are compared by pricingId 
instance Ord UserPricing where
  compare (UserPricing _ id1 _) (UserPricing _ id2 _) = compare id1 id2
  
             
instance FromJSON UserPricing
instance ToJSON UserPricing



data AdminPricing = AdminPricing {
  adminBkl :: T.Text,        -- Id of bookable
  adminPricing :: T.Text,    -- Id of pricing 
  roomIds :: S.Set T.Text
  } deriving (Show, Generic)
             
-- Two adminPricings are equal if their pricingIds are equal. This is made so in order
-- to avoid duplicates.
instance Eq AdminPricing where
  (AdminPricing _ id1 _) == (AdminPricing _ id2 _) = id1 == id2 
  
  
-- AdminPricings are compared by pricingId. 
instance Ord AdminPricing where
  compare (AdminPricing _ id1 _) (AdminPricing _ id2 _) = compare id1 id2             
            
instance FromJSON AdminPricing
instance ToJSON AdminPricing



data Assignment = Assignment {
  reservedPricing :: Integer, -- Id of ReservedPricing
  rooms :: S.Set T.Text
  } deriving (Show, Generic)
             
-- Two assignments are equal if their reservedPricingIds are equal. This is made so in order
-- to avoid duplicates.
instance Eq Assignment where
  (Assignment id1 _) == (Assignment id2 _) = id1 == id2 
  
  
-- Assignments are compared by reservedPricingId. 
instance Ord Assignment where
  compare (Assignment id1 _) (Assignment id2 _) = compare id1 id2  
             
instance FromJSON Assignment
instance ToJSON Assignment



data BookAvailability = BookAvailability {
  bookable :: T.Text,
  totalRooms :: S.Set T.Text,
  reservedRooms :: S.Set T.Text,
  availableRooms :: S.Set T.Text
} deriving (Show, Generic)


-- Two BookAvailabilities are equal if their bookIds are equal. This is made so in order
-- to avoid duplicates.
instance Eq BookAvailability where
  (BookAvailability id1 _ _ _) == (BookAvailability id2 _ _ _) = id1 == id2 
  
-- Bookavailabilities are compared by bookId. 
instance Ord BookAvailability where
  compare (BookAvailability id1 _ _ _) (BookAvailability id2 _ _ _) = compare id1 id2  
             
instance FromJSON BookAvailability
instance ToJSON BookAvailability



data Acceptance = Acceptance {
  message :: Maybe T.Text,
  assignments :: S.Set Assignment
  } deriving (Eq, Show, Generic)
  
instance FromJSON Acceptance
instance ToJSON Acceptance



data BasicReservData = BasicReservData {
  propId :: T.Text,  
  name :: T.Text,
  lastName :: T.Text,
  checkIn :: TM.UTCTime,
  checkOut :: TM.UTCTime,
  email :: Maybe T.Text,
  specialReqs :: Maybe T.Text,
  messageToHost :: Maybe T.Text,
  contactPhone :: Maybe T.Text
  } deriving (Eq, Show, Generic)
             
instance FromJSON BasicReservData
instance ToJSON BasicReservData



data AdminReservData = AdminReservData {
  adminReservation :: BasicReservData,
  adminPricings :: S.Set AdminPricing
  } deriving (Eq, Show, Generic)
             
instance FromJSON AdminReservData
instance ToJSON AdminReservData



data UserReservData = UserReservData {
  userReservation :: BasicReservData,
  userPricings :: S.Set UserPricing -- We use a set to avoid duplicated elements.
  } deriving (Eq, Show, Generic)
             
instance FromJSON UserReservData
instance ToJSON UserReservData



data Reservation = Reservation {
  reservId :: Integer,
  numNights :: Int,
  submitted :: TM.UTCTime,
  expiration :: TM.UTCTime,
  absence :: (TM.UTCTime, TM.UTCTime), -- (checkIn + 24h, checkOut)
  guestArrived :: Bool,
  resCode :: T.Text,
  state :: ReservState,
  byAdmin :: Bool, -- This tells us if the reservation was created manually by the Admin. 
  basicData :: BasicReservData
  } deriving (Show, Generic)
             
instance FromJSON Reservation
instance ToJSON Reservation

instance Eq Reservation where
  (Reservation id1 _ _ _ _ _ _ _ _ _) == (Reservation id2 _ _ _ _ _ _ _ _ _) =
    id1 == id2


-- The following data types are for notifying the potential guest about the state of his reservation:

data Notification = Notification {
  notifId :: Integer,   -- reservId_<alphanum_chars>
  reservation :: Integer,
  messageToGuest :: Maybe T.Text,
  notified :: TM.UTCTime,
  subject :: ReservState
  } deriving (Show, Generic)
             
instance FromJSON Notification
instance ToJSON Notification


instance Eq Notification where
  (Notification id1 _ _ _ _) == (Notification id2 _ _ _ _) =
    id1 == id2
