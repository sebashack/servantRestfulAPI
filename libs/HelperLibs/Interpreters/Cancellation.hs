{-# LANGUAGE OverloadedStrings #-}


module HelperLibs.Interpreters.Cancellation where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Servant
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import HelperLibs.Interpreters.BookingDomain
import HelperLibs.ElasticSearch.ResponseParser
import Control.Monad.Except
import Configs.ConfigTypes 
import HelperLibs.MySQL.ActionRunner
import Data.Monoid
import qualified Data.Text as T
import qualified Schemas.SQL.DbTypes as DbT
import qualified Domains.BookingDomain.Bookable.DataTypes as BT
import qualified Domains.BookingDomain.Reservation.DataTypes as RT
import qualified Domains.BookingDomain.Cancellation.DataTypes as CT
import qualified Schemas.SQL.User as USC
import qualified Schemas.SQL.Reservation as RSC 
import qualified Data.Map.Strict as Map
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
import qualified Database.Persist.MySQL as MySQL
import qualified Data.ByteString as SB (ByteString)
import qualified Data.Set as S

{-
  This function validates important facts about a CancellationData data type:
   -- userId matching.
   -- reservation and reservedPricing matching.
   -- valid number of cancelled rooms.
   -- valid states of reservation: Accepted or Pending.
   -- not expired or Accepted reservation.
   -- checkIn greater than current date.
   -- if the reservation was made by an admin or an ordinary user.
-}
validateCancData :: MonadIO m => ConfigES
                              -> Integer
                              -> Maybe Integer
                              -> CT.CancellationData
                              -> RSC.Reservation
                              -> RSC.ReservedPricing
                              -> (Bool -> Bool)
                              -> Either String (RT.ReservState, Int, [T.Text])
                              -> T.Text
                              -> ReaderT MySQL.SqlBackend m (Either String (RT.ReservState, Int, [T.Text]))
validateCancData coEs reservId userId cancData repoReserv repoResPri p leftErr token = do
  let (RSC.Reservation byAdmin userKey _ exp _ arrived _ state propId _ _ _ cIn cOut _ _ _) = repoReserv
      (RSC.ReservedPricing reservKey bookId  _ _ _ _ _ numRooms _ _ assignedRooms) = repoResPri
      state' = toDomainReservState state
      propId' = TE.decodeUtf8 propId
      userId' = fmap (toInteger .  MySQL.unSqlBackendKey . USC.unUserKey) userKey
      reservId' = reservKeyToInt reservKey 
      bookId' = TE.decodeUtf8 bookId
      assignedRooms' = S.toList $ parseRooms assignedRooms
  currentDate <- liftIO $ TM.getCurrentTime
  eitherBookable <- liftIO $ runExceptT $ queryAndParseBookable coEs bookId'
  case eitherBookable of
    Left error -> return $ Left error
    Right bookable -> do
      let propId'' = BT.propId $ BT.basicData bookable
          conditions = (userId == userId',
                        reservId == reservId',
                        propId' == propId'',
                        numCanc > 0,
                        numCanc <= numRooms, 
                        state' == RT.Pending || state' == RT.Accepted,
                        exp > currentDate || state' == RT.Accepted,
                        cIn > currentDate,
                        p byAdmin) 
      case conditions of
        (False, _, _, _, _, _, _, _, _) -> return userReservMismatch
        (_, False, _, _, _, _, _, _, _) -> return reservIdsMismatch 
        (_, _, False, _, _, _, _, _, _) -> return bookPropMismatch
        (_, _, _, False, _, _, _, _, _) -> return invalidCancelledRooms
        (_, _, _, _, False, _, _, _, _) -> return tooManyCancelledRooms
        (_, _, _, _, _, False, _, _, _) -> return invalidCancellationState
        (_, _, _, _, _, _, False, _, _) -> return cannotCancelExpired
        (_, _, _, _, _, _, _, False, _) -> return cannotCancelAfterCheckIn
        (_, _, _, _, _, _, _, _, True) -> return leftErr
        (True, True, True, True, True, True, True, True, False) -> return $ Right (state', numRooms, assignedRooms') 
  where  
    (CT.CancellationData resPriId numCanc) = cancData
      

-- Given a repoCancellation convert it into a domain Cancellation.
toDomainCancellation :: (Integer, RSC.Cancellation) -> CT.Cancellation
toDomainCancellation (cancId, canc) =
  CT.Cancellation reservId cancDate cancRooms' byAdmin (CT.CancellationData resPriId numRooms)
  where
    (RSC.Cancellation byAdmin cancDate reservKey resPriKey cancRooms) = canc
    reservId = reservKeyToInt reservKey
    resPriId =  toInteger $ MySQL.unSqlBackendKey (RSC.unReservedPricingKey resPriKey)
    cancRooms' = parseRooms (Just cancRooms)
    numRooms = S.size cancRooms'
