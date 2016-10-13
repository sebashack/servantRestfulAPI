{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Schemas.SQL.Reservation where

import Configs.MySQL 
import HelperLibs.MySQL.ActionRunner
import Schemas.SQL.DbTypes
import Schemas.SQL.User (UserId)
import Data.ByteString (ByteString)
import Data.Time
import qualified Data.Text as T
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Database.Persist as Per


-- This schema supports Cascade Deletions from Reservation.

TH.share [TH.mkPersist TH.sqlSettings, TH.mkDeleteCascade TH.sqlSettings, TH.mkMigrate "reservationModel"] [TH.persistLowerCase| 
Reservation
  byAdmin           Bool  -- this field tells us if the reservation was made by an admin.
  userId            UserId Maybe -- This can be NULL in case that the reservation was made by an admin. It is not exposed in reservations.
  submitted         UTCTime
  expiration        UTCTime 
  absence           UTCTime
  arrived           Bool
  code              ByteString Maybe 
  state             ReservState
  propertyId        ByteString
  name              T.Text
  lastName          T.Text
  email             T.Text Maybe
  checkIn           UTCTime 
  checkOut          UTCTime 
  specialReqs       T.Text Maybe
  messageToHost     T.Text Maybe
  contactPhone      T.Text Maybe
  UniqueReservCode  code !force
  deriving Show
ReservedPricing 
  reservId          ReservationId
  bookableId        ByteString
  bookableName      T.Text
  checkIn           UTCTime 
  checkOut          UTCTime 
  conditions        T.Text
  occupancy         Int
  numRooms          Int 
  nightPrice        Int -- BigInt
  discount          Int
  assignedRooms     T.Text Maybe -- Array which has been transformed into a Text representation which can be parsed.
  deriving Show     
HostNotification 
  reservId          ReservationId
  messageToGuest    T.Text Maybe
  created           UTCTime 
  subject           ReservState
  UniqueNotification reservId
  deriving Show 
Review
  reviewDate            UTCTime
  reservId              ReservationId
  content               T.Text
  score                 Int
  UniqueReservReview reservId
  deriving Show
Cancellation
  byAdmin           Bool
  cancelled         UTCTime
  reservId          ReservationId
  reservedPriId     ReservedPricingId
  cancelledRooms    T.Text -- An array which has been transformed into a Text representation which can be parsed.
  deriving Show
|]



printDevelRes :: IO ()
printDevelRes = runMySQL mysqlDevelConfig $ MySQL.printMigration reservationModel





