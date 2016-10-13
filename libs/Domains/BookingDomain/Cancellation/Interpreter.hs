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

module Domains.BookingDomain.Cancellation.Interpreter ( cancellationServer ) where


import Control.Monad.Except
import Network.Wai
import Servant
import Servant.JuicyPixels
import Data.Aeson
import Data.Aeson.Types
import Domains.BookingDomain.Cancellation.API
import Configs.ConfigTypes
import HelperLibs.MySQL.ActionRunner
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad (mapM)
import HelperLibs.ElasticSearch.ResponseParser
import HelperLibs.Interpreters.BookingDomain
import HelperLibs.Interpreters.Cancellation
import qualified Database.Persist.MySQL as MySQL
import qualified Schemas.SQL.DbTypes as DbT
import qualified Repositories.CancellationRepo.Operations as CR
import qualified Repositories.ReservationRepo.Operations as RR
import qualified Schemas.SQL.Reservation as RSC 
import qualified Schemas.SQL.User as USC 
import qualified Domains.BookingDomain.Cancellation.DataTypes as CT
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
  Given a reservationId and a CancellationData data type this endpoint stores, if necessary,
  a new cancellation by a user in DB for that reservation.
  This endpoint has the following conditions:
   - The Id provided in the token must match the userId in the reservation.
   - The reservedPricing in CancellationData must match the reservationId provided.
   - The number of cancelled units must be greater than 0 but less than the number of rooms
     in the reservedPricing.
   - The state of the reservation must be Pending or Accepted.
   - The reservation is either not expired or Accepted.
   - The checkIn of the reservation is greater than the current date.
   - The reservation must be one made by an ordinary user.
  Also, this endpoint has special behaviors:
   - If the reservation is Pending, no cancellation is stored in DB. The only thing that
     happens here is that the number of rooms in the reservedPricing is decremented. In
     case that the reservedPricing comes to have a number of rooms equal to 0, it will be
     deleted. Also, in case that the reservation comes to have 0 reservedPricings, that
     reservation will be deleted.
   - If the reservation is Accepted, the expected behavior occurs: the cancelled rooms
     are subtracted from the reservedPricing (both from the number of rooms and the assigned
     rooms) and the cancellation is stored in DB.
-}
postCancellationByUser :: ConfigMySQL
                       -> ConfigES
                       -> Integer
                       -> CT.CancellationData
                       -> Maybe T.Text
                       -> ExceptT ServantErr IO CT.Cancellation
postCancellationByUser _ _ _ _ Nothing = throwError _noToken
postCancellationByUser coMysql coEs reservId cancData (Just token) = do
  canc <- liftIO $ runMySQL coMysql $ do
    credentials <- validateUser token
    eitherReserv <- RR.getReservById reservId
    eitherResPri <- RR.getReservedPriById resPriId
    case (credentials, eitherReserv, eitherResPri) of
      (Left error, _, _) -> return $ Left error
      (_, Left _, _) -> return reservNotFound
      (_, _, Left _) -> return resPriNotFound
      (Right (userId, _, _, _, _), Right repoReserv, Right repoResPri) -> do
        cancValidation <- validateCancData coEs reservId (Just userId) cancData repoReserv repoResPri id cannotCancelByAdmin token
        case cancValidation of
          Left error -> return $ Left error
          Right (state, numRooms, assignedRooms) -> do
            if state == RT.Pending
            then do
              currentDate <- liftIO $ TM.getCurrentTime
              let cancellation = CT.Cancellation reservId currentDate S.empty False cancData              
              case numRooms - numCanc < 1 of
                True -> do
                  RR.deleteReservedPricing resPriId
                  resPriCount <- RR.getReservPricingsCount reservId
                  if resPriCount < 1
                  then do
                    RR.deleteReservation reservId
                    return $ Right cancellation
                  else do
                    return $ Right cancellation
                False -> do  
                  RR.subtractReservedPriNumRooms resPriId numCanc
                  return $ Right cancellation                      
            else do
              let updatedRooms = drop numCanc assignedRooms
                  droppedRooms = take numCanc assignedRooms
              RR.subtractReservedPriNumRooms resPriId numCanc
              RR.updateReservedPriRooms resPriId updatedRooms
              (cancId, repoCanc) <- CR.storeCancellation False reservId resPriId droppedRooms
              let cancDate = RSC.cancellationCancelled repoCanc
                  cancellation = CT.Cancellation reservId cancDate (S.fromList droppedRooms) False cancData 
              return $ Right cancellation
  either (\err -> throwError err) (\v -> return v) (getCustomError canc)              
  where  
    (CT.CancellationData resPriId numCanc) = cancData




{-
  Given a reservationId and a CancellationData data type this endpoint stores a new cancellation in DB by an admin
  for that reservation.
  This endpoint has the following conditions:
   - The reservation must have a null userId.
   - The reservedPricing in CancellationData must match the reservationId provided.
   - The number of cancelled units must be greater than 0 but less than the number of rooms
     in the reservedPricing.
   - The state of the reservation must be Pending or Accepted.
   - The reservation is either not expired or Accepted.
   - The checkIn of the reservation is greater than the current date.
   - The reservation must be one made by an admin.
  the cancelled rooms are subtracted from the reservedPricing (both from the number of rooms and the assigned
  rooms) and the cancellation is stored in DB. 
 -}
postCancellationByAdmin :: ConfigMySQL
                        -> ConfigES
                        -> Integer
                        -> CT.CancellationData
                        -> Maybe T.Text
                        -> ExceptT ServantErr IO CT.Cancellation
postCancellationByAdmin _ _ _ _ Nothing = throwError _noToken
postCancellationByAdmin coMysql coEs reservId cancData (Just token) = do
  canc <- liftIO $ runMySQL coMysql $ do
    eitherReserv <- RR.getReservById reservId
    eitherResPri <- RR.getReservedPriById resPriId
    case (eitherReserv, eitherResPri) of
      (Left _, _) -> return reservNotFound
      ( _, Left _) -> return resPriNotFound
      (Right repoReserv, Right repoResPri) -> do
        let propId = TE.decodeUtf8 $ RSC.reservationPropertyId repoReserv
        adminCreds <- validateAdminAndMatchProperty coEs token propId
        cancValidation <- validateCancData coEs reservId Nothing cancData repoReserv repoResPri not cannotCancelByUser token
        case (adminCreds, cancValidation) of
          (Left error, _) -> return $ Left error
          (_, Left error) -> return $ Left error
          (Right _, Right (_, _,assignedRooms)) -> do
            let updatedRooms = drop numCanc assignedRooms
                droppedRooms = take numCanc assignedRooms
            RR.subtractReservedPriNumRooms resPriId numCanc
            RR.updateReservedPriRooms resPriId updatedRooms
            (cancId, repoCanc) <- CR.storeCancellation True reservId resPriId droppedRooms
            let cancDate = RSC.cancellationCancelled repoCanc
                cancellation = CT.Cancellation reservId cancDate (S.fromList droppedRooms) True cancData 
            return $ Right cancellation
  either (\err -> throwError err) (\v -> return v) (getCustomError canc)              
  where  
    (CT.CancellationData resPriId numCanc) = cancData

  

-- Given a reservationId return all the cancellations related to that reservation.
-- These cancellations can be seen either by the reservation's user or by the property admin.
getReservCancellations :: ConfigMySQL
                       -> ConfigES
                       -> Integer
                       -> Maybe T.Text
                       -> ExceptT ServantErr IO [CT.Cancellation]
getReservCancellations _ _ _ Nothing = throwError _noToken
getReservCancellations coMysql coEs reservId (Just token) = do
  cancs <- liftIO $ runMySQL coMysql $ do
    match <- matchUserAndReserv coMysql coEs reservId token
    case match of
      Left error -> return $ Left error
      Right _ -> do
        repoCancs <- CR.getReservCancellations reservId
        return $ Right (fmap toDomainCancellation repoCancs)
  either (\err -> throwError err) (\v -> return v) (getCustomError cancs)    



-- Given a propertyId get all cancellations related to that property. This are only
-- available to the property admin.
getPropertyCancellations :: ConfigMySQL
                         -> ConfigES
                         -> T.Text
                         -> Maybe Int
                         -> Maybe Int
                         -> Maybe T.Text
                         -> ExceptT ServantErr IO [CT.Cancellation]
getPropertyCancellations _  _  _  _  _ Nothing = throwError _noToken
getPropertyCancellations coMysql coEs propId from size (Just token) = do
  cancs <- liftIO $ runMySQL coMysql $ do
    match <- validateAdminAndMatchProperty coEs token propId
    case match of
      Left error -> return $ Left error
      Right _ -> do
        let from' =  fromIntegral $ maybe 0 id from
            size' = fromIntegral $ maybe 15 id size
        repoCancs <- CR.getPropCancellations propId from' size'
        return $ Right (fmap toDomainCancellation repoCancs)
  either (\err -> throwError err) (\v -> return v) (getCustomError cancs)


    
cancellationServer :: ConfigMySQL -> ConfigES -> Server CancellationAPI
cancellationServer coMysql coEs = getReservCancellations coMysql coEs
                             :<|> postCancellationByUser coMysql coEs
                             :<|> postCancellationByAdmin coMysql coEs
                             :<|> getPropertyCancellations coMysql coEs
                        

  
