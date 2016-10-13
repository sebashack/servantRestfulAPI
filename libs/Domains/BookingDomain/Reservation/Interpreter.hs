{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domains.BookingDomain.Reservation.Interpreter ( reservationServer ) where

import Control.Monad.Except
import Network.Wai
import Servant
import Servant.JuicyPixels
import Data.Aeson
import Data.Aeson.Types
import Domains.BookingDomain.Reservation.API
import Configs.ConfigTypes
import HelperLibs.MySQL.ActionRunner
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad (mapM)
import HelperLibs.ElasticSearch.ResponseParser
import HelperLibs.Interpreters.Reservation
import HelperLibs.Interpreters.BookingDomain
import qualified HelperLibs.SCalendar.Operations as SC
import qualified HelperLibs.SCalendar.DataTypes as SCT
import qualified Database.Persist.MySQL as MySQL
import qualified Schemas.SQL.DbTypes as DbT
import qualified Repositories.ReservationRepo.Operations as RR
import qualified Schemas.SQL.Reservation as RSC 
import qualified Domains.BookingDomain.Reservation.DataTypes as RT
import qualified Domains.BookingDomain.Bookable.DataTypes as BT
import qualified Data.ByteString.Lazy as LB (toStrict, fromStrict)
import qualified Data.ByteString as SB (length, ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
import qualified Data.Set as S


{-
  Given a userToken and a ReservData post a new reservation related to the user identified by that token.
  Note that there are many validations going on here:
    - User credentials are validated.
    - BasicReservData is validated.
    - UserPricings are validated.
    - Calendar Availability for each bookable is validated.
-}
postReservationByUser :: ConfigMySQL
                      -> ConfigES
                      -> Maybe T.Text
                      -> RT.UserReservData
                      -> ExceptT ServantErr IO RT.Reservation
postReservationByUser _ _ Nothing _ = throwError _noToken
postReservationByUser coMysql coEs (Just token) (RT.UserReservData reservInfo userPris) = do
  reservCreation <- liftIO $ runMySQL coMysql $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, profName, mainEmail, _, _) -> do
        eitherReservData <- liftIO $ runExceptT $ validateBasicReservData reservInfo (Just mainEmail)
        case eitherReservData of
          Left error -> return $ Left error
          Right validatedData -> do
            let (RT.BasicReservData propId fName lName cIn cOut email sReqs msg phNum) = validatedData
            eitherPricings <- liftIO $ runExceptT $ mapM (matchUserPricingAndProperty coEs propId) (S.toList userPris)
            case eitherPricings of
              Left error -> return $ Left error
              Right pricings -> do
                validateAvailabilityAndStoreReserv cIn cOut validatedData (Just userId)
                                                   pricings
                                                   (mergeNumberOfRooms pricings) -- merge number rooms to validate. 
                                                   checkAvailability
                                                   storePricingInfo
  either (\err -> throwError err) (\v -> return v) (getCustomError reservCreation)
  where 
    toCalendar from to (bookId, _, roomIds, numRooms, _) = do
      maybeCalendar <- createBookableCalendar bookId from to 128 roomIds
      case maybeCalendar of
        Nothing -> return (numRooms, SCT.SCalendar roomIds (SCT.Empty (from, to)))
        Just calendar -> return (numRooms, calendar)
    checkAvailability from to (numRooms, calendar) =  SC.isQuantityAvailable numRooms (from, to) calendar
    storePricingInfo rId from to (bookId, bookName, roomIds, numRooms, BT.Pricing priId priData) = do
      RR.storeReservedPricing rId bookId from to [] bookName conds occu numRooms price disc
      return ()
      where
        (BT.PricingData occu conds price disc) = priData



{-
  Given a userToken and a ReservData post a new reservation as an admin, which means that
  no userId is stored.
  Note that there are many validations going on here:
    - Admin credentials are validated.
    - BasicReservData is validated.
    - AdminPricings are validated.
    - Calendar Availability for each bookable is validated: Note that for each Bookable we must
      validate that the desired quantity is available and that the specific rooms which were assigned
      are available.
-}
postReservationByAdmin :: ConfigMySQL
                       -> ConfigES
                       -> Maybe T.Text
                       -> RT.AdminReservData
                       -> ExceptT ServantErr IO RT.Reservation
postReservationByAdmin _ _ Nothing _ = throwError _noToken
postReservationByAdmin coMysql coEs (Just token) (RT.AdminReservData reservInfo adminPris) = do
  reservCreation <- liftIO $ runMySQL coMysql $ do
    let propId = RT.propId reservInfo
    adminCreds <- validateAdminAndMatchProperty coEs token propId
    case adminCreds of
      Left error -> return $ Left error
      Right (adminId, _, _, _, _, _, _, _) -> do
        eitherReservData <- liftIO $ runExceptT $ validateBasicReservData reservInfo Nothing 
        case eitherReservData of
          Left error -> return $ Left error
          Right validatedData -> do
            let (RT.BasicReservData propId fName lName cIn cOut email sReqs msg phNum) = validatedData
            eitherPricings <- liftIO $ runExceptT $ mapM (matchAdminPricingAndProperty coEs propId) (S.toList adminPris)
            case eitherPricings of
              Left error -> return $ Left error
              Right pricings -> do
                validateAvailabilityAndStoreReserv cIn cOut validatedData Nothing
                                                   pricings
                                                   (mergeAssignedRooms pricings) -- merge the assigned rooms to validate.
                                                   checkAssignedRoomsAvailability
                                                   storePricingInfo                             
  either (\err -> throwError err) (\v -> return v) (getCustomError reservCreation)
  where
    storePricingInfo rId from to (bookId, bookName, roomIds, assignedRooms, BT.Pricing priId priData) = do
      RR.storeReservedPricing rId bookId from to (S.toList assignedRooms) bookName conds occu (S.size assignedRooms) price disc
      return ()
      where
        (BT.PricingData occu conds price disc) = priData

    


-- Given a token and a reservationId, get a reservation which matches either the reservation's
-- user or the reservation's admin.
getReservation :: ConfigMySQL
               -> ConfigES
               -> Integer
               -> Maybe T.Text
               -> ExceptT ServantErr IO RT.Reservation
getReservation _ _ _ Nothing = throwError _noToken
getReservation coMysql coEs reservId (Just token) = do
  reservation <- liftIO $ runMySQL coMysql $ matchUserAndReserv coMysql coEs reservId token 
  either (\err -> throwError err) (\v -> return v) (getCustomError reservation)      
 



-- Given a reservationId get the information of the reservedPricings related to that
-- reservation. This endpoint must match either the reservation's
-- user or the reservation's admin. 
getPricingInfo :: ConfigMySQL
               -> ConfigES
               -> Integer
               -> Maybe T.Text
               -> ExceptT ServantErr IO [RT.PricingInfo]
getPricingInfo _ _ _ Nothing = throwError _noToken                
getPricingInfo coMysql coEs reservId (Just token) = do
  resPris <- liftIO $ runMySQL coMysql $ do
    repoReserv <- matchUserAndReserv coMysql coEs reservId token
    case repoReserv of
      Left error -> return $ Left error
      Right reservation -> do
        repoResPris <- RR.getReservPricings reservId
        return $ Right (fmap toDomainResPri repoResPris) 
  either (\err -> throwError err) (\v -> return v) (getCustomError resPris)            
  



-- Given a propertyId get all reservations related to a property which are in a given
-- ReservState. Note that the state ReservState is determined according to the function
-- setDefaultState. Also, you can specify the pagination (from, size) of the result.
getPropertyReservations :: ConfigMySQL
                        -> ConfigES
                        -> T.Text
                        -> Maybe T.Text
                        -> Maybe Int
                        -> Maybe Int
                        -> Maybe T.Text
                        -> ExceptT ServantErr IO [RT.Reservation]
getPropertyReservations _ _ _ _ _ _ Nothing = throwError _noToken                          
getPropertyReservations coMysql coEs propId state from size (Just token) = do
  reservations <- liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdminAndMatchProperty coEs token propId
    case adminCreds of
      Left error -> return $ Left error
      Right _ -> do
        let state' = setDefaultState state
            from' =  fromIntegral $ maybe 0 (setBound 0) from
            size' = fromIntegral $ maybe 15 (setBound 15) size
        repoReservs <- RR.getPropReservations propId state' from' size'
        return $ Right (fmap toDomainReserv repoReservs)
  either (\err -> throwError err) (\v -> return v) (getCustomError reservations)      
 

    

-- Given a userId get all reservations related to that user which are in a given
-- ReservState. Note that the state ReservState is determined according to the function
-- setDefaultState. Also, you can specify the pagination (from, size) of the result.    
getUserReservations :: ConfigMySQL
                    -> Maybe T.Text
                    -> Maybe Int
                    -> Maybe Int
                    -> Maybe T.Text
                    -> ExceptT ServantErr IO [RT.Reservation]
getUserReservations _ _ _ _ Nothing = throwError _noToken
getUserReservations coMysql state from size (Just token) = do
  reservations <- liftIO $ runMySQL coMysql $ do
    userCreds <- validateUser token 
    case userCreds of
      Left error -> return $ Left error
      Right (userId, _,  _, _, _) -> do
        let state' = setDefaultState state
            from' =  fromIntegral $ maybe 0 (setBound 0) from
            size' = fromIntegral $ maybe 15 (setBound 15) size
        repoReservs <- RR.getUserReservations userId state' from' size'
        return $ Right (fmap toDomainReserv repoReservs)
  either (\err -> throwError err) (\v -> return v) (getCustomError reservations)




-- Given a bookableId get information about all reservedPricings related to that bookable.
-- Note that the reservedPricings are related to a reservation in a given state.
-- You can also specify the pagination (from, size) of the result.
getBookReservInfo :: ConfigMySQL
                  -> ConfigES
                  -> T.Text
                  -> Maybe T.Text
                  -> Maybe Int
                  -> Maybe Int
                  -> Maybe T.Text
                  -> ExceptT ServantErr IO [RT.PricingInfo]
getBookReservInfo _ _ _ _ _ _ Nothing = throwError _noToken         
getBookReservInfo coMysql coEs bookId state from size (Just token) = do
  resPris <- liftIO $ runMySQL coMysql $ do
    validation <-validateAdminAndMatchBookable coEs token bookId
    case validation of
      Left error -> return $ Left error
      Right _ -> do
        let state' = setDefaultState state
            from' =  fromIntegral $ maybe 0 (setBound 0) from
            size' = fromIntegral $ maybe 15 (setBound 15) size
        repoResPris <- RR.getBklReservedPris bookId state' from' size'
        return $ Right (fmap toDomainResPri repoResPris)     
  either (\err -> throwError err) (\v -> return v) (getCustomError resPris)




{-
  Given a reservationId and an Acceptance accept a reservation and assign rooms to every reservedPricing of
  that reservation. This endpoint has the following conditions:
   - The the reservation property and the admin of that property must match.
   - The reservation must be one made by an admin.
   - The reservation must be in Pending state.
   - The reservation cannot be expired.
   - Every assignment must be validated by matchAdminPricingAndProperty
   - The total LIST of rooms for each bookable must be validated according to checkAssignedRoomsAvailability
   - The count of assignments must match the count of reservedPris. Note that assignments is a set ordered by resPriId,
     so this condition is enough to enforce a matching.

  This endpoint changes the state of the reservation to Accepted and creates a Notification.
  Note that the optional message is validated, an if it does not pass validation, a null is stored.
-}  
acceptReservation :: ConfigMySQL
                  -> ConfigES
                  -> Integer
                  -> RT.Acceptance
                  -> Maybe T.Text
                  -> ExceptT ServantErr IO RT.Notification
acceptReservation _ _ _ _ Nothing = throwError _noToken                  
acceptReservation coMysql coEs reservId (RT.Acceptance msg assignments) (Just token) = do
  notification <- liftIO $ runMySQL coMysql $ do
    repoReserv <- RR.getReservById reservId
    case repoReserv of
      Left error -> return $ reservNotFound
      Right reserv -> do
        let (RSC.Reservation byAdmin _ _ exp _ _ _ state propId _ _ _ cIn cOut _ _ _) = reserv
            state' = toDomainReservState state         
            propId' = TE.decodeUtf8 propId
        reservPrisCount <- RR.getReservPricingsCount reservId    
        currentDate <- liftIO $ TM.getCurrentTime    
        adminValidation <- validateAdminAndMatchProperty coEs token propId'    
        case (adminValidation, byAdmin, state' == RT.Pending, exp > currentDate, S.size assignments == reservPrisCount) of
          (Left error, _, _, _, _) -> return $ Left error
          (_, True, _, _, _) -> return cannotAcceptByAdmin
          (_, _, False, _, _) -> return cannotAcceptNotPending
          (_, _, _, False, _) -> return cannotAcceptExpired
          (_, _, _, _, False) -> return assignmentsResPrisMismatch -- assignments and resPris must match
          (Right _, False, True, True, True) -> do
            eitherAssignments <- mapM (validateAssignment coEs token propId' reservId) (S.toList assignments)
            case sequence eitherAssignments of
                Left error -> return $ Left error
                Right validAssignments -> do
                  let start = TM.addUTCTime (-2764800) cIn     
                      end = TM.addUTCTime (2764800) cOut
                      merged = mergeAssignedRooms validAssignments 
                      lastNight = TM.addUTCTime (-86400) cOut -- check availability up to the last night of the reservation
                  calendars <- mapM (tupleToCalendar start end) $ removeDuplicateBkls merged
                  if all (checkAssignedRoomsAvailability cIn lastNight) calendars == False
                  then return roomsNotAvailable
                  else do
                    let msg' = msg >>= validateDescriptionM
                    mapM updateResPri (S.toList assignments)
                    RR.updateReservationState reservId DbT.Accepted
                    (notifId, repoNotif) <- RR.storeHostNotification reservId msg' DbT.Accepted
                    let notified = RSC.hostNotificationCreated repoNotif
                    return $ Right (RT.Notification notifId reservId msg' notified RT.Accepted) 
  either (\err -> throwError err) (\v -> return v) (getCustomError notification)
  where
    updateResPri (RT.Assignment rPId rooms) = RR.updateReservedPriRooms rPId (S.toList rooms) 
  


{-
  Given a reservationId and an optional message reject a reservation. This endpoint has the following conditions:
   - The the reservation property and the admin of that property must match.
   - The reservation must be one made by a user.
   - The reservation must be in Pending state.
   - The reservation cannot be expired.
  This endpoint creates a Notification with the optional message if provided.
  Note that the optional message is validated, an if it does not pass validation, a null is stored.
-}  
rejectReservation :: ConfigMySQL
                  -> ConfigES
                  -> Integer
                  -> T.Text
                  -> Maybe T.Text
                  -> ExceptT ServantErr IO RT.Notification
rejectReservation _ _ _ _ Nothing = throwError _noToken                  
rejectReservation coMysql coEs reservId msg (Just token) = do
  notification <- liftIO $ runMySQL coMysql $ do
    repoReserv <- RR.getReservById reservId
    case repoReserv of
      Left error -> return $ reservNotFound
      Right reserv -> do
        let (RSC.Reservation byAdmin _ _ exp _ _ _ state propId _ _ _ cIn cOut _ _ _) = reserv
            state' = toDomainReservState state         
            propId' = TE.decodeUtf8 propId
        currentDate <- liftIO $ TM.getCurrentTime    
        adminValidation <- validateAdminAndMatchProperty coEs token propId'    
        case (adminValidation, byAdmin, state' == RT.Pending, exp > currentDate) of
          (Left error, _, _, _) -> return $ Left error
          (_, True, _, _) -> return cannotRejectByAdmin
          (_, _, False, _) -> return cannotRejectNotPending
          (_, _, _, False) -> return cannotRejectExpired
          (Right _, False, True, True) -> do
            let msg' = Just msg >>= validateDescriptionM
            RR.updateReservationState reservId DbT.Rejected
            (notifId, repoNotif) <- RR.storeHostNotification reservId msg' DbT.Rejected
            let notified = RSC.hostNotificationCreated repoNotif
            return $ Right (RT.Notification notifId reservId msg' notified RT.Rejected) 
  either (\err -> throwError err) (\v -> return v) (getCustomError notification)
 



{-
  Given a reservationId and an optional message absent a reservation. This endpoint has the following conditions:
   - The the reservation property and the admin of that property must match.
   - The reservation must be one made by a user.
   - The reservation must be in Accepted state.
   - The reservation cannot be marked as arrived.
   - The current date must be in the valid absence interval, that is, (checkIn + 24h, checkOut)
  This endpoint creates a Notification with the optional message if provided.
  Note that the optional message is validated, an if it does not pass validation, a null is stored.
-}
absentUserReservation :: ConfigMySQL
                      -> ConfigES
                      -> Integer
                      -> T.Text
                      -> Maybe T.Text
                      -> ExceptT ServantErr IO RT.Notification
absentUserReservation _ _ _ _ Nothing = throwError _noToken
absentUserReservation coMysql coEs reservId msg (Just token) = do
  notification <- liftIO $ runMySQL coMysql $ do
    repoReserv <- RR.getReservById reservId
    case repoReserv of
      Left error -> return $ reservNotFound
      Right reserv -> do
        currentDate <- liftIO $ TM.getCurrentTime
        let (RSC.Reservation byAdmin _ _ _ absence arrived _ state propId _ _ _ cIn cOut _ _ _) = reserv
            state' = toDomainReservState state         
            propId' = TE.decodeUtf8 propId
            isAbsent = currentDate >= absence && currentDate <= cOut 
        adminValidation <- validateAdminAndMatchProperty coEs token propId'    
        case (adminValidation, byAdmin, arrived, state' == RT.Accepted, isAbsent) of
          (Left error, _, _, _, _) -> return $ Left error
          (_, True, _, _, _) -> return cannotAbsentByAdmin
          (_, _, True, _, _) -> return cannotAbsentArrived  
          (_, _,  _, False, _) -> return cannotAbsentNotAccepted
          (_, _, _, _, False) -> return invalidAbsentDate
          (Right _, False, False, True, True) -> do
            let msg' = Just msg >>= validateDescriptionM
            RR.updateReservationState reservId DbT.Absent
            (notifId, repoNotif) <- RR.storeHostNotification reservId msg' DbT.Absent
            let notified = RSC.hostNotificationCreated repoNotif
            return $ Right (RT.Notification notifId reservId msg' notified RT.Absent) 
  either (\err -> throwError err) (\v -> return v) (getCustomError notification)
 



{-
  Given a reservationId and an optional message absent a reservation. This endpoint has the following conditions:
   - The the reservation property and the admin of that property must match.
   - The reservation must be one made by an admin.
   - The reservation must be in Accepted state.
   - The reservation cannot be marked as arrived.
   - The current date must be in the valid absence interval, that is, (checkIn + 24h, checkOut)
  This endpoint creates a Notification with the optional message if provided.
  Note that the optional message is validated, an if it does not pass validation, a null is stored.
-}
absentAdminReservation :: ConfigMySQL
                       -> ConfigES
                       -> Integer
                       -> Maybe T.Text
                       -> ExceptT ServantErr IO ()
absentAdminReservation _ _ _ Nothing = throwError _noToken
absentAdminReservation coMysql coEs reservId (Just token) = do
  notification <- liftIO $ runMySQL coMysql $ do
    repoReserv <- RR.getReservById reservId
    case repoReserv of
      Left error -> return $ reservNotFound
      Right reserv -> do
        currentDate <- liftIO $ TM.getCurrentTime
        let (RSC.Reservation byAdmin _ _ _ absence arrived _ state propId _ _ _ cIn cOut _ _ _) = reserv
            state' = toDomainReservState state         
            propId' = TE.decodeUtf8 propId
            isAbsent = currentDate >= absence && currentDate <= cOut 
        adminValidation <- validateAdminAndMatchProperty coEs token propId'    
        case (adminValidation, byAdmin, arrived, state' == RT.Accepted, isAbsent) of
          (Left error, _, _, _, _) -> return $ Left error
          (_, False, _, _, _) -> return cannotAbsentNotByAdmin
          (_, _, True, _, _) -> return cannotAbsentArrived  
          (_, _,  _, False, _) -> return cannotAbsentNotAccepted
          (_, _, _, _, False) -> return invalidAbsentDate
          (Right _, True, False, True, True) -> do
            RR.updateReservationState reservId DbT.Absent
            return $ Right ()
  either (\err -> throwError err) (\v -> return v) (getCustomError notification)



  
-- Given a user authentication token get all the notifications related to that user.
-- You can provide pagination parameters (from, size).
getUserNotifications :: ConfigMySQL 
                     -> Maybe Int
                     -> Maybe Int
                     -> Maybe T.Text
                     -> ExceptT ServantErr IO [RT.Notification]
getUserNotifications _ _ _ Nothing = throwError _noToken                     
getUserNotifications coMysql from size (Just token) = do
  notifications <- liftIO $ runMySQL coMysql $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, _, _, _, _) -> do
        let from' = maybe 0 (setBound 0) from
            size' = maybe 15 (setBound 15) size
        repoNotifs <- RR.getUserNotifications userId from' size'
        return $ Right (fmap toDomainNotif repoNotifs)
  either (\err -> throwError err) (\v -> return v) (getCustomError notifications)


    

-- Given a notificationId get that notification if the user is validated.
getUserNotification :: ConfigMySQL
                    -> Integer
                    -> Maybe T.Text
                    -> ExceptT ServantErr IO RT.Notification
getUserNotification _  _ Nothing = throwError _noToken                     
getUserNotification coMysql notifId (Just token) = do
  notification <- liftIO $ runMySQL coMysql $ do
    credentials <- validateUser token
    case credentials of
      Left error -> return $ Left error
      Right (userId, _, _, _, _) -> do
        eitherNotif <- RR.getHostNotificationById notifId
        case eitherNotif of
          Left error -> return notifNotFound
          Right repoNotif -> do
            let notif = toDomainNotif (notifId, repoNotif)
                reservId = RT.reservation notif
            eitherReserv <- RR.getReservById reservId
            case eitherReserv of
              Left error -> return reservNotFound
              Right reserv -> do
                let userId' = fmap userKeyToInt (RSC.reservationUserId reserv)
                case Just userId == userId' of
                  False -> return userNotifMismatch
                  True -> return $ Right notif
  either (\err -> throwError err) (\v -> return v) (getCustomError notification)




-- Given a reservation code get the reservation identified with that code. Note that
-- this endpoint is only allowed for admins.
getReservationByCode :: ConfigMySQL
                     -> ConfigES
                     -> T.Text
                     -> Maybe T.Text
                     -> ExceptT ServantErr IO RT.Reservation
getReservationByCode _ _ _ Nothing = throwError _noToken  
getReservationByCode coMysql coEs code (Just token) = do
  reservation <- liftIO $ runMySQL coMysql $ do
    eitherReserv <- RR.getReservByCode code
    case eitherReserv of
      Left error -> return reservNotFound
      Right repoReserv -> do
        let reserv = toDomainReserv repoReserv
            propId = RT.propId $ RT.basicData reserv
        adminCreds <- validateAdminAndMatchProperty coEs token propId
        either (\err -> return $ Left err) (\_ -> return $ Right reserv) adminCreds
  either (\err -> throwError err) (\v -> return v) (getCustomError reservation)




{-
  Given a reservationId mark that reservation as arrived. This endpoint has the following conditions:
   - The the reservation property and the admin of that property must match.
   - The reservation must be one made by a user.
   - The reservation must be in Accepted state.
   - The reservation cannot be marked as arrived.
   - The current date must be creater than checkIn.
-}
updateGuestArrival :: ConfigMySQL
                   -> ConfigES
                   -> Integer
                   -> Maybe T.Text
                   -> ExceptT ServantErr IO ()
updateGuestArrival _ _ _ Nothing = throwError _noToken
updateGuestArrival coMysql coEs reservId (Just token) = do
  reservation <- liftIO $ runMySQL coMysql $ do
    eitherReserv <- RR.getReservById reservId
    case eitherReserv of
      Left error -> return reservNotFound
      Right repoReserv -> do
        let (RT.Reservation _ _ _ _ _ arrived _ state _ basicData) = toDomainReserv (reservId, repoReserv)
            propId = RT.propId basicData
            checkIn = RT.checkIn basicData
        currentDate <- liftIO $ TM.getCurrentTime
        adminValidation <- validateAdminAndMatchProperty coEs token propId
        case (adminValidation, state == RT.Accepted, arrived, currentDate >= checkIn) of
          (Left error, _, _, _) -> return $ Left error
          (_, False, _, _) -> return cannotArriveNotAccepted
          (_, _, True, _) -> return alreadyArrived 
          (_, _, _, False) -> return cannotArriveBeforeCin
          (Right _, True, False, True) -> do
            RR.updateReservationArrival reservId True
            return $ Right ()
  either (\err -> throwError err) (\v -> return v) (getCustomError reservation)




-- Given a propertyId get all the reservations of that property which can be set as
-- Absent. The admin of the property must be validated.
getAbsentableReservs :: ConfigMySQL
                     -> ConfigES
                     -> T.Text
                     -> Maybe Int
                     -> Maybe Int
                     -> Maybe T.Text
                     -> ExceptT ServantErr IO [RT.Reservation]
getAbsentableReservs _ _ _ _ _ Nothing = throwError _noToken                     
getAbsentableReservs coMysql coEs propId from size (Just token) = do
  reservations <- liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdminAndMatchProperty coEs token propId
    case adminCreds of
      Left error -> return $ Left error
      Right _ -> do
        let from' = maybe 0 (setBound 0) from
            size' = maybe 15 (setBound 15) size
        repoReservs <- RR.getValidAbsentPropReservs propId from' size'
        return $ Right (fmap toDomainReserv repoReservs)
  either (\err -> throwError err) (\v -> return v) (getCustomError reservations)





-- Given a reservationId, get a list of BookAvailabilities which tells the admin the available rooms
-- for each bookable in the period of time of that reservation.
getReservAvailability :: ConfigMySQL
                      -> ConfigES
                      -> Integer
                      -> Maybe T.Text
                      -> ExceptT ServantErr IO [RT.BookAvailability]
getReservAvailability _ _ _ Nothing = throwError _noToken
getReservAvailability coMysql coEs reservId (Just token) = do
  reports <- liftIO $ runMySQL coMysql $ do
    eitherReserv <- RR.getReservById reservId
    case eitherReserv of
      Left error -> return $ reservNotFound
      Right repoReserv -> do
        let (RT.Reservation _ _ _ exp _ arrived _ state _ basicData) = toDomainReserv (reservId, repoReserv)
            propId = RT.propId basicData
            cIn = RT.checkIn basicData
            cOut = RT.checkOut basicData
        currentDate <- liftIO $ TM.getCurrentTime
        adminValidation <- validateAdminAndMatchProperty coEs token propId
        case (adminValidation, state == RT.Pending, exp > currentDate, currentDate > cIn) of
          (Left error, _, _, _) -> return $ Left error
          (_, False, _, _) -> return $ Right []
          (_, _, False, _) -> return $ Right []
          (_, _, _, True) -> return $ Right []
          (Right _, True, True, False) -> do
            repoResPris <- RR.getReservPricings reservId
            let bookIds = S.toList $ S.fromList $ fmap (RT.bklId . toDomainResPri) repoResPris
                start = TM.addUTCTime (-2764800) cIn     
                end = TM.addUTCTime (2764800) cOut
                lastNight = TM.addUTCTime (-86400) cOut -- create reports up to the last night of the reservation
            maybeCalendars <- mapM (createCalendar coEs start end 128) bookIds
            case sequence maybeCalendars of
              Nothing -> return $ calendarCreationErr
              Just calendars -> do
               let  maybeReports = mapM (createReport (cIn, lastNight)) calendars
               maybe (return $ reportCreationErr) (\rs -> return $ Right (fmap reportToAvailability rs)) maybeReports
  either (\err -> throwError err) (\v -> return v) (getCustomError reports)             
  where
    createCalendar config  from to numDays bookId = do
      eitherBookable <- liftIO $ runExceptT $ queryAndParseBookable config bookId
      case eitherBookable of
        Left error -> return Nothing
        Right bookable -> do
          let roomIds = BT.roomIds $ BT.basicData bookable 
          calendar <- createBookableCalendar bookId from to numDays roomIds
          maybe (return Nothing) (\v -> return $ Just (bookId, v)) calendar
    createReport (l, u) (bookId, calendar) = do     
      report <- SC.periodReport (l, u) calendar
      return (bookId, report)
    reportToAvailability (bookId, SCT.Report _ total reserved remaining) =
      RT.BookAvailability bookId total reserved remaining



      
reservationServer :: ConfigMySQL -> ConfigES -> Server ReservationAPI
reservationServer coMysql coEs = (getReservation coMysql coEs)
                            :<|> (getPricingInfo coMysql coEs)
                            :<|> (postReservationByUser coMysql coEs) 
                            :<|> (postReservationByAdmin coMysql coEs)
                            :<|> (getPropertyReservations coMysql coEs)
                            :<|> (getUserReservations coMysql)
                            :<|> (getBookReservInfo coMysql coEs)
                            :<|> (acceptReservation coMysql coEs)
                            :<|> (rejectReservation coMysql coEs)
                            :<|> (absentUserReservation coMysql coEs)
                            :<|> (absentAdminReservation coMysql coEs)
                            :<|> (getUserNotifications coMysql)
                            :<|> (getUserNotification coMysql)
                            :<|> (getReservationByCode coMysql coEs)
                            :<|> (updateGuestArrival coMysql coEs)
                            :<|> (getAbsentableReservs coMysql coEs)
                            :<|> (getReservAvailability coMysql coEs)
