{-# LANGUAGE OverloadedStrings #-}

module Repositories.ReservationRepo.Operations where


import Data.Time
import HelperLibs.MySQL.ReservationRepo 
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString) 
import qualified Schemas.SQL.User as USCH
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


-- Given the basic information for a reservation, store that reservation in DB. If no userId is given, it is
-- assumed that the reservation was made by an admin.
-- Also note that if the reservation was made by an admin its state defaults to Accepted, otherwise it defaults 
-- to Pending
storeReservBasicData :: MonadIO m => T.Text
                                  -> Maybe Integer
                                  -> T.Text
                                  -> T.Text
                                  -> TM.UTCTime
                                  -> TM.UTCTime
                                  -> Maybe T.Text
                                  -> Maybe T.Text
                                  -> Maybe T.Text
                                  -> Maybe T.Text
                                  -> ReaderT MySQL.SqlBackend m (Integer, SCH.Reservation) 
storeReservBasicData propId mUserId gName gLastName cIn cOut gEmail sReqs msgToHost cPhone = do
  currentDate <- liftIO TM.getCurrentTime   
  let expDate = TM.addUTCTime 86400 currentDate -- A reservation request expires after 24h
      absenceDate = TM.addUTCTime 86400 cIn  -- A guest can be regarded as absent in an interval (checkIn + 24h, checkOut)
      reservation = SCH.Reservation isAdmin
                                    mUserKey
                                    currentDate
                                    expDate
                                    absenceDate
                                    False    -- By default the guest is asumed as not arrived.
                                    Nothing
                                    reservState
                                    (TE.encodeUtf8 propId)
                                    gName
                                    gLastName
                                    gEmail
                                    cIn
                                    cOut
                                    sReqs
                                    msgToHost
                                    cPhone
  reservKey <- Per.insert reservation
  let reservId = toInteger $ MySQL.unSqlBackendKey (SCH.unReservationKey reservKey)
  reservCode <- liftIO $ genReservCode reservId
  Per.update reservKey [SCH.ReservationCode Per.=. Just reservCode]
  return (reservId, reservation { SCH.reservationCode = Just reservCode })
  where
    isAdmin = maybe True (\v -> False) mUserId 
    reservState = maybe DbT.Accepted (\v -> DbT.Pending) mUserId
    mUserKey = case mUserId of
      Nothing -> Nothing
      Just v -> Just $ USCH.UserKey $ MySQL.SqlBackendKey (fromInteger v)




-- Update the state of a reservation

updateReservationState :: MonadIO m => Integer
                                    -> DbT.ReservState
                                    -> ReaderT MySQL.SqlBackend m ()
updateReservationState reservId state = do
  let key =  SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)  
  Per.update key [SCH.ReservationState Per.=. state]
  


-- Update a reservation's arrival

updateReservationArrival :: MonadIO m => Integer
                                      -> Bool
                                      -> ReaderT MySQL.SqlBackend m ()
updateReservationArrival reservId arrived = do
  let key =  SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)  
  Per.update key [SCH.ReservationArrived Per.=. arrived]



-- Given the information of a pricing related to a bookable and a reservation, this function stores it in the data
-- base.
storeReservedPricing :: MonadIO m => Integer
                                  -> T.Text
                                  -> TM.UTCTime
                                  -> TM.UTCTime
                                  -> [T.Text]
                                  -> T.Text
                                  -> [T.Text]
                                  -> Int
                                  -> Int
                                  -> Integer
                                  -> Int
                                  -> ReaderT MySQL.SqlBackend m (Integer, SCH.ReservedPricing)
storeReservedPricing reservId bklId cIn cOut assignedRms bklName conds occu numRooms price disc  = do
  let reservKey = SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)
      reservedPri = SCH.ReservedPricing reservKey
                                        (TE.encodeUtf8 bklId)
                                        bklName
                                        cIn
                                        cOut
                                        (T.pack $ show conds) -- Text that can be parsed with read
                                        occu
                                        numRooms
                                        (fromInteger price)
                                        disc
                                        maybeRooms -- Text that can be parsed with read.
  reservedPriKey <- Per.insert reservedPri
  let reservedPriId = toInteger $ MySQL.unSqlBackendKey (SCH.unReservedPricingKey reservedPriKey)
  return (reservedPriId, reservedPri)
  where
    maybeRooms = case assignedRms of
      [] -> Nothing
      rs -> Just $ T.pack (show rs)




-- Update the assigned rooms for a reserved pricing.
updateReservedPriRooms :: MonadIO m => Integer
                                    -> [T.Text]
                                    -> ReaderT MySQL.SqlBackend m ()
updateReservedPriRooms reservPriId assignedRms = do
  let key =  SCH.ReservedPricingKey $ MySQL.SqlBackendKey (fromInteger reservPriId)  
  Per.update key [SCH.ReservedPricingAssignedRooms Per.=. maybeRooms]
  where
    maybeRooms = case assignedRms of
      [] -> Nothing
      rs -> Just $ T.pack (show rs)


-- Update the number of rooms for a reserved pricing.
subtractReservedPriNumRooms :: MonadIO m => Integer
                                       -> Int
                                       -> ReaderT MySQL.SqlBackend m ()
subtractReservedPriNumRooms reservPriId cancRooms = do
  let key =  SCH.ReservedPricingKey $ MySQL.SqlBackendKey (fromInteger reservPriId)  
  Per.update key [SCH.ReservedPricingNumRooms Per.-=. (fromIntegral cancRooms)] 



-- Get a reservation by Id
getReservById :: MonadIO m => Integer -> ReaderT MySQL.SqlBackend m (Either String SCH.Reservation)
getReservById reservId = do
  let reservKey = SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)
  maybeReservation <- Per.get reservKey
  case maybeReservation of
    Nothing -> return $ Left ("There's no reservation with id " ++ (show reservId))
    Just reservation -> return $ Right reservation 




-- Get a reservation by reservation code
getReservByCode :: MonadIO m => T.Text -> ReaderT MySQL.SqlBackend m (Either String (Integer, SCH.Reservation))
getReservByCode reservCode = do
  maybeReservation <- Per.getBy $ SCH.UniqueReservCode (Just $ TE.encodeUtf8 reservCode)
  case maybeReservation of
    Nothing -> return $ Left ("There's no reservation with code " ++ (T.unpack reservCode))
    Just entity -> return $ Right (reservEntityToTuple entity) 
    
    

-- Get a reservedPricing by Id.
getReservedPriById :: MonadIO m => Integer -> ReaderT MySQL.SqlBackend m (Either String SCH.ReservedPricing)
getReservedPriById resPriId = do
  let resPriKey = SCH.ReservedPricingKey $ MySQL.SqlBackendKey (fromInteger resPriId)
  maybeResPri <- Per.get resPriKey
  case maybeResPri of
    Nothing -> return $ Left ("There's no reservedPricing with id " ++ (show resPriId))
    Just resPri -> return $ Right resPri 



-- Given a reservation Id get all its associated reservedPricings

getReservPricings :: MonadIO m => Integer
                               -> ReaderT MySQL.SqlBackend m [(Integer, SCH.ReservedPricing)]
getReservPricings reservId = do
  let reservKey = SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)  
  reservedPris <- (DBEsq.select $ DBEsq.from (\(resPris) -> do
                     DBEsq.where_ (resPris DBEsq.^. SCH.ReservedPricingReservId
                                   DBEsq.==. DBEsq.val reservKey)
                     return $ resPris))
  return $ fmap reservedPrisEntityToTuple reservedPris


-- Check if the given profileName is available in DB.
getReservPricingsCount :: MonadIO m => Integer
                                    -> ReaderT MySQL.SqlBackend m Int
getReservPricingsCount reservId = do
  let reservKey = SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)  
  Per.count [SCH.ReservedPricingReservId Per.==. reservKey]

    

-- Given a propertyId get all its associated reservations
-- The reservations will be ordered from the most recent to the oldest.
-- ReservState is a filter so that you get only reservations in a given state.
getPropReservations :: MonadIO m => T.Text
                                 -> DbT.ReservState
                                 -> Integer
                                 -> Integer
                                 -> ReaderT MySQL.SqlBackend m [(Integer, SCH.Reservation)]
getPropReservations propId state from size = do
  reservations <- (DBEsq.select $ DBEsq.from (\(reservs) -> do
                    (DBEsq.offset $ fromInteger from)
                    (DBEsq.limit $ fromInteger size)
                    (DBEsq.orderBy [DBEsq.desc (reservs DBEsq.^. SCH.ReservationSubmitted)])
                    DBEsq.where_ (          reservs DBEsq.^. SCH.ReservationPropertyId
                                            DBEsq.==. DBEsq.val (TE.encodeUtf8 propId)
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationState 
                                            DBEsq.==. DBEsq.val state           )
                    return $ reservs))
  return $ fmap reservEntityToTuple reservations
  


-- Given a userId get all its associated reservations
-- The reservations will be ordered from the most recent to the oldest.
-- ReservState is a filter so that you get only reservations in a given state. 
getUserReservations :: MonadIO m => Integer
                                 -> DbT.ReservState
                                 -> Integer
                                 -> Integer
                                 -> ReaderT MySQL.SqlBackend m [(Integer, SCH.Reservation)]
getUserReservations userId state from size = do  
  let userKey = USCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  reservations <- (DBEsq.select $ DBEsq.from (\(reservs) -> do
                    (DBEsq.offset $ fromInteger from)
                    (DBEsq.limit $ fromInteger size)
                    (DBEsq.orderBy [DBEsq.desc (reservs DBEsq.^. SCH.ReservationSubmitted)])
                    DBEsq.where_ (          reservs DBEsq.^. SCH.ReservationUserId
                                            DBEsq.==. DBEsq.val (Just userKey)
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationState 
                                            DBEsq.==. DBEsq.val state)
                    return $ reservs))
  return $ fmap reservEntityToTuple reservations
  




-- Given a bookableId get all its associated reservedPricings
-- ReservState is a filter so that the ReservedPricings queried will only come
-- from reservations which are in a specific state: Pending, Rejected, Accepted or Absent
getBklReservedPris :: MonadIO m => T.Text
                                -> DbT.ReservState
                                -> Integer
                                -> Integer
                                -> ReaderT MySQL.SqlBackend m [(Integer, SCH.ReservedPricing)]
getBklReservedPris bklId state from size = do
   reservedPris <- (DBEsq.select $ DBEsq.from (\(resPris `DBEsq.InnerJoin` reservs) -> do
                     (DBEsq.offset $ fromInteger from)
                     (DBEsq.limit $ fromInteger size)
                     (DBEsq.orderBy [DBEsq.desc (reservs DBEsq.^. SCH.ReservationSubmitted)])
                     DBEsq.on (resPris DBEsq.^. SCH.ReservedPricingReservId  
                               DBEsq.==. reservs DBEsq.^. SCH.ReservationId)
                     DBEsq.where_ (          reservs DBEsq.^. SCH.ReservationState 
                                             DBEsq.==. DBEsq.val state
                                   DBEsq.&&. resPris DBEsq.^. SCH.ReservedPricingBookableId
                                             DBEsq.==. DBEsq.val (TE.encodeUtf8 bklId)    )
                     return resPris))
   return $ fmap reservedPrisEntityToTuple reservedPris
 




-- Store a HostNotification

storeHostNotification :: MonadIO m => Integer
                                   -> Maybe T.Text
                                   -> DbT.ReservState
                                   -> ReaderT MySQL.SqlBackend m (Integer, SCH.HostNotification)
storeHostNotification reservId message subject = do
  currentDate <- liftIO TM.getCurrentTime
  let reservKey = SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)
      notification = SCH.HostNotification reservKey
                                          message
                                          currentDate
                                          subject
  notifKey <- Per.insert notification
  let notifId = toInteger $ MySQL.unSqlBackendKey (SCH.unHostNotificationKey notifKey)
  return (notifId, notification)




-- Get a HostNotification by Id
getHostNotificationById :: MonadIO m => Integer
                                     -> ReaderT MySQL.SqlBackend m (Either String SCH.HostNotification)
getHostNotificationById notifId = do
   let notifKey = SCH.HostNotificationKey $ MySQL.SqlBackendKey (fromInteger notifId)
   maybeNotification <- Per.get notifKey
   case maybeNotification of
     Nothing -> return $ Left ("There's no notification with id " ++ (show notifId))
     Just notification -> return $ Right notification 



-- Get all the notifications that hosts have adressed to some user 
getUserNotifications :: MonadIO m => Integer
                                  -> Int
                                  -> Int
                                  -> ReaderT MySQL.SqlBackend m [(Integer, SCH.HostNotification)]
getUserNotifications userId from size = do
  let userKey = USCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  notifs <- (DBEsq.select $ DBEsq.from (\(hostNotifs `DBEsq.InnerJoin` reservs `DBEsq.InnerJoin` users) -> do
              (DBEsq.offset $ fromIntegral from)
              (DBEsq.limit $ fromIntegral size)
              (DBEsq.orderBy [DBEsq.desc (hostNotifs DBEsq.^. SCH.HostNotificationCreated)])
              DBEsq.on (DBEsq.just (users DBEsq.^.USCH.UserId)
                        DBEsq.==. reservs DBEsq.^. SCH.ReservationUserId)
              DBEsq.on (reservs DBEsq.^. SCH.ReservationId
                        DBEsq.==. hostNotifs DBEsq.^. SCH.HostNotificationReservId)
              DBEsq.where_ (users DBEsq.^. USCH.UserId
                            DBEsq.==. DBEsq.val userKey)
              return hostNotifs))                         
  return $ fmap entityToTuple notifs
  where
   entityToTuple (Per.Entity key entity) =
     let eId = toInteger $ MySQL.unSqlBackendKey (SCH.unHostNotificationKey key)
     in (eId, entity)

                                  


-- Get all ReservedPricings of a Property which are in a given state and included in
-- a given period, that is, the ReservedPricings returned will be associated with 
-- reservations whose submission date is within the time interval provided.
getPropReservedPrisInPeriod :: MonadIO m => T.Text
                                         -> DbT.ReservState
                                         -> (TM.UTCTime, TM.UTCTime) 
                                         -> ReaderT MySQL.SqlBackend m [(Integer, SCH.ReservedPricing)]
getPropReservedPrisInPeriod propId state (lower, upper) = do
  reservedPris <- (DBEsq.select $ DBEsq.from (\(resPris `DBEsq.InnerJoin` reservs) -> do
                    (DBEsq.orderBy [DBEsq.desc (reservs DBEsq.^. SCH.ReservationSubmitted)])
                    DBEsq.on (resPris DBEsq.^. SCH.ReservedPricingReservId  
                              DBEsq.==. reservs DBEsq.^. SCH.ReservationId)
                    DBEsq.where_ (          reservs DBEsq.^. SCH.ReservationState 
                                            DBEsq.==. DBEsq.val state
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationPropertyId
                                            DBEsq.==. DBEsq.val (TE.encodeUtf8 propId)
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationSubmitted
                                            DBEsq.>=. DBEsq.val lower
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationSubmitted
                                            DBEsq.<=. DBEsq.val upper                )
                    return resPris))
  return$ fmap reservedPrisEntityToTuple reservedPris
  





-- Get all ReservedPricings of a Bookable which are in a given state and after
-- given date, that is, the ReservedPricings returned will be associated with 
-- reservations whose checkOut date is after the date provided.
-- Note that reservations are ordered from most recent to oldest.
getBklReservedPrisAfter :: MonadIO m => T.Text
                                     -> DbT.ReservState
                                     -> TM.UTCTime 
                                     -> ReaderT MySQL.SqlBackend m [(Integer, SCH.ReservedPricing)]
getBklReservedPrisAfter bklId state date = do
  reservedPris <- (DBEsq.select $ DBEsq.from (\(resPris `DBEsq.InnerJoin` reservs) -> do
                    (DBEsq.orderBy [DBEsq.desc (reservs DBEsq.^. SCH.ReservationSubmitted)])
                    DBEsq.on (resPris DBEsq.^. SCH.ReservedPricingReservId  
                              DBEsq.==. reservs DBEsq.^. SCH.ReservationId)
                    DBEsq.where_ (          reservs DBEsq.^. SCH.ReservationState 
                                            DBEsq.==. DBEsq.val state
                                  DBEsq.&&. resPris DBEsq.^. SCH.ReservedPricingBookableId
                                            DBEsq.==. DBEsq.val (TE.encodeUtf8 bklId)
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationCheckOut
                                            DBEsq.>=. DBEsq.val date                     )
                    return resPris))
  return $ fmap reservedPrisEntityToTuple reservedPris




-- Get all ReservedPricings of a Bookable which are in a given state and included in
-- a given period, that is, the ReservedPricings returned will be associated with 
-- reservations whose (checIn,checkOut) interval is within the time interval provided.
-- Note that reservations are ordered from most recent to oldest.
getBklReservedPrisInPeriod_ :: MonadIO m => T.Text
                                        -> DbT.ReservState
                                        -> (TM.UTCTime, TM.UTCTime) 
                                        -> ReaderT MySQL.SqlBackend m [(Integer, SCH.ReservedPricing)]
getBklReservedPrisInPeriod_ bklId state (lower, upper) = do
  reservedPris <- (DBEsq.select $ DBEsq.from (\(resPris `DBEsq.InnerJoin` reservs) -> do
                    (DBEsq.orderBy [DBEsq.desc (reservs DBEsq.^. SCH.ReservationSubmitted)])
                    DBEsq.on (resPris DBEsq.^. SCH.ReservedPricingReservId  
                              DBEsq.==. reservs DBEsq.^. SCH.ReservationId)
                    DBEsq.where_ (          reservs DBEsq.^. SCH.ReservationState 
                                            DBEsq.==. DBEsq.val state
                                  DBEsq.&&. resPris DBEsq.^. SCH.ReservedPricingBookableId
                                            DBEsq.==. DBEsq.val (TE.encodeUtf8 bklId)
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationCheckIn
                                            DBEsq.>=. DBEsq.val lower
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationCheckIn
                                            DBEsq.<=. DBEsq.val upper
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationCheckOut
                                            DBEsq.>=. DBEsq.val lower          
                                  DBEsq.&&. reservs DBEsq.^. SCH.ReservationCheckOut
                                            DBEsq.<=. DBEsq.val upper                )
                    return resPris))
  return $ fmap reservedPrisEntityToTuple reservedPris



from = TM.UTCTime (TM.fromGregorian 2016 10 19) 0
to = TM.UTCTime (TM.fromGregorian 2016 11 07) 0

pipo :: IO [(Integer, SCH.ReservedPricing)]
pipo = runMySQL mysqlDevelConfig $ getBklReservedPrisInPeriod_ "AVdA2Kmb88A8iqhIB58y" DbT.Accepted (from, to)


-- Get all reservations of a Property whose state can be changed to Absent.
-- Results are ordered from most recent to oldest.
getValidAbsentPropReservs :: MonadIO m => T.Text
                                       -> Int
                                       -> Int
                                       -> ReaderT MySQL.SqlBackend m [(Integer, SCH.Reservation)]  
getValidAbsentPropReservs propId from size = do
  currentDate <- liftIO $ TM.getCurrentTime 
  reservations <- (DBEsq.select $ DBEsq.from (\(reservs) -> do
                     (DBEsq.offset $ fromIntegral from)
                     (DBEsq.limit $ fromIntegral size)
                     (DBEsq.orderBy [DBEsq.desc (reservs DBEsq.^. SCH.ReservationSubmitted)])
                     DBEsq.where_ (          reservs DBEsq.^. SCH.ReservationPropertyId
                                             DBEsq.==. DBEsq.val (TE.encodeUtf8 propId)
                                   DBEsq.&&. reservs DBEsq.^. SCH.ReservationState 
                                             DBEsq.==. DBEsq.val DbT.Accepted
                                   DBEsq.&&. reservs DBEsq.^. SCH.ReservationAbsence 
                                             DBEsq.<=. DBEsq.val currentDate
                                   DBEsq.&&. reservs DBEsq.^. SCH.ReservationCheckOut 
                                             DBEsq.>=. DBEsq.val currentDate
                                   DBEsq.&&. reservs DBEsq.^. SCH.ReservationArrived 
                                             DBEsq.==. DBEsq.val False             )
                     return $ reservs))
  return $ fmap reservEntityToTuple reservations
   



-- Given a reservedPricingId, delete it. 
deleteReservedPricing :: MonadIO m => Integer -> ReaderT MySQL.SqlBackend m ()
deleteReservedPricing resPriId = do
  let key =  SCH.ReservedPricingKey $ MySQL.SqlBackendKey (fromInteger resPriId)
  Per.delete key  
  return ()



-- Given a reservId, delete that reservation. This deletion is
-- in cascade, that is, the Reservation together with its ReservedPricings,
-- Cancellations and  Reviews are deleted.
deleteReservation :: MonadIO m => Integer -> ReaderT MySQL.SqlBackend m ()
deleteReservation reservId = do
  let key =  SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)
  Per.deleteCascade key  
  return ()



-- Given a period, this function deletes reservations whose submission date is less than
-- the given period. This deletion is in cascade, that is, the reservation together with
-- its ReservedPricings, Cancellations and Reviews are deleted.
deleteReservationsBefore :: MonadIO m => TM.UTCTime -> ReaderT MySQL.SqlBackend m ()
deleteReservationsBefore date = do
  Per.deleteCascadeWhere [SCH.ReservationSubmitted Per.<. date]                          
  return ()






