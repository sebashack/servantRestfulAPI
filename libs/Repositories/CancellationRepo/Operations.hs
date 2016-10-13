{-# LANGUAGE OverloadedStrings #-}

module Repositories.CancellationRepo.Operations where


import Data.Time
import HelperLibs.MySQL.CancellationRepo 
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Schemas.SQL.DbTypes as DbT
import qualified Data.Time as TM
import qualified Schemas.SQL.Reservation as SCH
import qualified Database.Esqueleto  as DBEsq
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Persist as Per

import HelperLibs.MySQL.ActionRunner
import Configs.MySQL 


-- Given the basic information for a cancellation, store that cancellation in DB.

storeCancellation :: MonadIO m => Bool
                               -> Integer
                               -> Integer
                               -> [T.Text]
                               -> ReaderT MySQL.SqlBackend m (Integer, SCH.Cancellation) 
storeCancellation byAdmin reservId reservedPriId cancRooms = do
  currentDate <- liftIO TM.getCurrentTime   
  let reservKey =  SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)
      reservedPriKey =  SCH.ReservedPricingKey $ MySQL.SqlBackendKey (fromInteger reservedPriId)
      rooms = (T.pack . show) cancRooms
      cancellation = SCH.Cancellation byAdmin
                                      currentDate
                                      reservKey
                                      reservedPriKey
                                      rooms
  cancKey <- Per.insert cancellation
  let cancId = toInteger $ MySQL.unSqlBackendKey (SCH.unCancellationKey cancKey)
  return (cancId, cancellation)



-- Given a reservationId return all the cancellations associated with it
getReservCancellations :: MonadIO m => Integer
                                    -> ReaderT MySQL.SqlBackend m [(Integer, SCH.Cancellation)]
getReservCancellations reservId = do
  let reservKey =  SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)
  cancellations <- (DBEsq.select $ DBEsq.from (\(cancs) -> do
                     (DBEsq.orderBy [DBEsq.desc (cancs DBEsq.^. SCH.CancellationCancelled)])
                     DBEsq.where_ (cancs DBEsq.^. SCH.CancellationReservId
                                   DBEsq.==. DBEsq.val reservKey)
                     return $ cancs))
  return $ fmap cancEntityToTuple cancellations
  where
   
    

-- Given a propertyId get all its associated cancellations.
-- The cancellations are given from the most recent to the oldest.
getPropCancellations :: MonadIO m => T.Text
                                  -> Integer
                                  -> Integer
                                  -> ReaderT MySQL.SqlBackend m [(Integer, SCH.Cancellation)]
getPropCancellations propId from size = do
   cancellations <- (DBEsq.select $ DBEsq.from (\(cancs `DBEsq.InnerJoin` reservs) -> do
                      (DBEsq.offset $ fromInteger from)
                      (DBEsq.limit $ fromInteger size)
                      (DBEsq.orderBy [DBEsq.desc (cancs DBEsq.^. SCH.CancellationCancelled)])
                      DBEsq.on (cancs DBEsq.^. SCH.CancellationReservId  
                                DBEsq.==. reservs DBEsq.^. SCH.ReservationId)
                      DBEsq.where_ (reservs DBEsq.^. SCH.ReservationPropertyId 
                                    DBEsq.==. DBEsq.val (TE.encodeUtf8 propId))
                      return cancs))
   return $ fmap cancEntityToTuple cancellations


-- Given a bookableId and an interval (from, to) this functions returns the pricing information of all cancellations 
-- belonging to that bookable whose reservedPricings' (checkIn, checkOut) is included in that period of time.
getBklCancInfoInPeriod :: MonadIO m => T.Text
                                    -> (TM.UTCTime, TM.UTCTime) 
                                    -> ReaderT MySQL.SqlBackend m [(T.Text, TM.UTCTime, TM.UTCTime, Int, Int)]
getBklCancInfoInPeriod bklId (lower, upper) = do
   cancellations <- (DBEsq.select $ DBEsq.from (\(cancs `DBEsq.InnerJoin` resPris) -> do
                      (DBEsq.orderBy [DBEsq.desc (cancs DBEsq.^. SCH.CancellationCancelled)])
                      DBEsq.on ( cancs DBEsq.^. SCH.CancellationReservedPriId
                                       DBEsq.==. resPris DBEsq.^. SCH.ReservedPricingId  )
                      DBEsq.where_ (          resPris DBEsq.^. SCH.ReservedPricingBookableId
                                              DBEsq.==. DBEsq.val (TE.encodeUtf8 bklId)   
                                    DBEsq.&&. resPris DBEsq.^. SCH.ReservedPricingCheckIn
                                              DBEsq.>=. DBEsq.val lower
                                    DBEsq.&&. resPris DBEsq.^. SCH.ReservedPricingCheckIn
                                              DBEsq.<=. DBEsq.val upper
                                    DBEsq.&&. resPris DBEsq.^. SCH.ReservedPricingCheckOut
                                              DBEsq.>=. DBEsq.val lower          
                                    DBEsq.&&. resPris DBEsq.^. SCH.ReservedPricingCheckOut
                                              DBEsq.<=. DBEsq.val upper                   )
                      return (cancs DBEsq.^. SCH.CancellationCancelledRooms,
                              resPris DBEsq.^. SCH.ReservedPricingCheckIn,
                              resPris DBEsq.^. SCH.ReservedPricingCheckOut,
                              resPris DBEsq.^. SCH.ReservedPricingNightPrice,
                              resPris DBEsq.^. SCH.ReservedPricingDiscount)))
   return $ fmap (\(DBEsq.Value v1,DBEsq.Value v2,DBEsq.Value v3,DBEsq.Value v4,DBEsq.Value v5) -> (v1,v2,v3,v4,v5)) cancellations
     

from = TM.UTCTime (TM.fromGregorian 2016 11 01) 0
to = TM.UTCTime (TM.fromGregorian 2016 11 07) 0

pipo :: IO [(T.Text, TM.UTCTime, TM.UTCTime, Int, Int)]
pipo = runMySQL mysqlDevelConfig $ getBklCancInfoInPeriod "AVdA2Kmb88A8iqhIB58y" (from, to)



