{-# LANGUAGE OverloadedStrings #-}


module HelperLibs.Interpreters.Reservation where

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
import qualified HelperLibs.SCalendar.Operations as SC
import qualified HelperLibs.SCalendar.DataTypes as SCT
import qualified Schemas.SQL.User as USC
import qualified Data.Text as T
import qualified Domains.BookingDomain.Reservation.DataTypes as RT
import qualified Domains.BookingDomain.Bookable.DataTypes as BT
import qualified Repositories.ReservationRepo.Operations as RR
import qualified Schemas.SQL.DbTypes as DbT
import qualified Schemas.SQL.Reservation as RSC 
import qualified Data.Map.Strict as Map
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
import qualified Database.Persist.MySQL as MySQL
import qualified Repositories.BookableRepo.Operations as BR (queryBookableById)
import qualified Data.ByteString as SB (ByteString)
import qualified Data.Set as S


-- HELPERS FOR MERGING PRICINGS --

groupSameBkls :: [(T.Text, T.Text, S.Set T.Text, a, BT.Pricing)]
              -> T.Text
              -> [(T.Text, T.Text, S.Set T.Text, a, BT.Pricing)]        
groupSameBkls ps bklId  = go ps []
  where
    first (bookId, _, _, _, _) = bookId
    go [] result = result
    go (e:es) result
      | bklId == first e = go es (e:result)
      | otherwise = go es result

    
getUniqueBookIds :: [(T.Text, T.Text, S.Set T.Text, a, BT.Pricing)] -> [T.Text]
getUniqueBookIds ps = go (toBookIds ps) []
  where
    toBookIds = fmap (\(bookId, _, _, _, _) -> bookId)
    go [] result = result
    go (e:es) result
      | elem e result = go es result
      | otherwise = go es (e:result)

-- END OF HELPERS FOR MERGING PRICINGS --



-- Given a pricingId and a list of Pricings, find that pricing.
findPricing :: T.Text -> [BT.Pricing] -> Maybe BT.Pricing
findPricing pId [] = Nothing 
findPricing pId (p:ps)
  | BT.priId p == pId = Just p
  | otherwise = findPricing pId ps




-- Given a propertyId and a UserPricing check if that UserPricing has a bookable that matches that property,
-- and that the pricingId provided matches, in turn, that bookable.
-- This function returns a tuple of the form (bookId, bookName, roomIds, numRooms, pricing).
matchUserPricingAndProperty :: ConfigES
                            -> T.Text
                            -> RT.UserPricing
                            -> ExceptT String IO (T.Text, T.Text, S.Set T.Text, Int, BT.Pricing)
matchUserPricingAndProperty coEs propId (RT.UserPricing bookId priId numRooms) = do
  bookable <- queryAndParseBookable coEs bookId
  let (BT.Bookable _ _ status bookData pricings) = bookable
      (BT.BasicBookableData propId' specs _ _ _  roomIds) = bookData
      bookName = BT.name specs
      conditions = (propId == propId',
                    numRooms > 0,
                    numRooms <= S.size roomIds,
                    status == BT.Listed,
                    findPricing priId  (S.toList pricings))
  case conditions of
    (False, _, _, _, _) -> ExceptT $ return bookPropMismatch
    (_, False, _, _, _) -> ExceptT $ return invalidNumRooms
    (_, _, False, _, _) -> ExceptT $ return tooManyRooms
    (_, _, _, False, _) -> ExceptT $ return unlistedBook
    (_, _, _, _, Nothing) -> ExceptT $ return priNotFound
    (True, True, True, True, Just pricing) -> return (bookId, bookName, roomIds, numRooms, pricing) 
  where
    getPricing pId [] = Nothing 
    getPricing pId (p:ps)
      | BT.priId p == pId = Just p
      | otherwise = getPricing pId ps




-- Given a propertyId and an AdminPricing check if that AdminPricing has a bookable that matches that property,
-- and that the pricingId provided matches, in turn, that bookable.
-- This function returns a tuple of the form (bookId, bookName, roomIds, numRooms, pricing).
-- Note that when it is the admin who makes the reservation, we do not require that the bookable be
-- listed.
matchAdminPricingAndProperty :: ConfigES
                            -> T.Text
                            -> RT.AdminPricing
                            -> ExceptT String IO (T.Text, T.Text, S.Set T.Text, S.Set T.Text, BT.Pricing)
matchAdminPricingAndProperty coEs propId (RT.AdminPricing bookId priId assignedRooms) = do
  bookable <- queryAndParseBookable coEs bookId
  let (BT.Bookable _ _ status bookData pricings) = bookable
      (BT.BasicBookableData propId' specs _ _ _  roomIds) = bookData
      bookName = BT.name specs
      conditions = (propId == propId',
                    not $ null  assignedRooms,
                    S.isSubsetOf assignedRooms roomIds,
                    findPricing priId  (S.toList pricings))
  case conditions of
    (False, _, _, _) -> ExceptT $ return bookPropMismatch
    (_, False, _, _) -> ExceptT $ return emptyAssignedRooms
    (_, _, False, _) -> ExceptT $ return badAssignedRooms
    (_, _, _, Nothing) -> ExceptT $ return priNotFound
    (True, True, True, Just pricing) -> return (bookId, bookName, roomIds, assignedRooms, pricing)     



{-
Validate an Assignment according to the following conditions:
  - The Assigment's reservedPricing must be found in DB.
  - the reservationId provided must match the Assignment's reservedPricing.
  - The number of rooms in the Assigment's reservedPricing must be the same as
    the size of the set of assigned rooms.
  - The Assigment's reservedPricing must not have assigned rooms, that is, it must have a
    null value (Nothing).
  - The propertyId provided must match the reservedPricing's bookable's propertyId
  - The Assingment's assignedRooms must be a subset of the roomIds of its reservedPricing's bookable.

The return type of this function has some dummy values (the second and last members of the tuple) so that we can
use other functions on it.
Note that we do not require that the bookables are listed in order to validate an assignment.
-}  
validateAssignment :: MonadIO m => ConfigES
                                -> T.Text
                                -> T.Text
                                -> Integer
                                -> RT.Assignment
                                -> ReaderT MySQL.SqlBackend m (Either String (T.Text, T.Text, S.Set T.Text, S.Set T.Text, BT.Pricing))
validateAssignment coEs token propId reservId (RT.Assignment resPriId assignedRooms) = do
  eitherResPri <- RR.getReservedPriById resPriId
  case eitherResPri of
    Left error -> return resPriNotFound
    Right resPri -> do
      let (RSC.ReservedPricing reservKey byteString  _ cIn cOut _ _ numRooms _ _ assignedRooms') = resPri
          bookId = TE.decodeUtf8 byteString
          reservId' = resPriKeyToInt reservKey 
          conditions1 = (reservId == reservId', S.size assignedRooms == numRooms, assignedRooms')
      case conditions1 of
        (False, _, _) -> return reservIdsMismatch
        (_, False, _) -> return assignedRoomsSizeMismatch
        (_, _, Just _) -> return notNullAssignedRooms        
        (True, True, Nothing) -> do
          eitherBookable <- liftIO $ runExceptT $ queryAndParseBookable coEs bookId
          case eitherBookable of
            Left error -> return $ Left error
            Right bookable -> do
              let (BT.Bookable _ _ _ bookData _) = bookable
                  (BT.BasicBookableData propId' _ _ _ _  roomIds) = bookData
                  conditions2 = (propId == propId', S.isSubsetOf assignedRooms roomIds) 
              case conditions2 of
                (False, _) -> return bookPropMismatch
                (_, False) -> return badAssignedRooms            
                (True, True) -> do
                  let dummyPri = BT.Pricing "" (BT.PricingData 0 [] 0 0)  
                  return $ Right (bookId, "", roomIds, assignedRooms, dummyPri)
  where
    resPriKeyToInt key = toInteger $ MySQL.unSqlBackendKey (RSC.unReservationKey key)
                       


                         
-- Given a list of tuples of the form (bookId, bookName, roomIds, numRooms, pricing), return a list of
-- the same type but where tuples with the same bookId have a merged numRooms, that is, the sum
-- of all numRooms with the same bookId.
mergeNumberOfRooms :: [(T.Text, T.Text, S.Set T.Text, Int, BT.Pricing)]
                   -> [(T.Text, T.Text, S.Set T.Text, Int, BT.Pricing)]
mergeNumberOfRooms prisInfo =
  let groups = fmap (groupSameBkls prisInfo) (getUniqueBookIds prisInfo)
  in concat $ fmap merge groups 
  where
    toNumRooms = fmap (\(_, _, _, numRooms, _) -> numRooms)
    merge :: [(T.Text, T.Text, S.Set T.Text, Int, BT.Pricing)]
          -> [(T.Text, T.Text, S.Set T.Text, Int, BT.Pricing)] 
    merge ps = fmap (updateSum (sum $ toNumRooms ps)) ps
      where
        updateSum s (bookId, bookName, roomIds, numRooms, pricing)
          = (bookId, bookName, roomIds, s, pricing)


    

-- Given a list of tuples of the form (bookId, bookName, roomIds, set, pricing), return a list of
-- tuples of the form (bookId, bookName, roomIds', list, pricing) where roomIds' is the result of 
-- merging the sets into a list of all tuples with the same bookId.
-- We have to change sets to lists because a list keeps duplicates, so we have information about
-- the total number of rooms that will be reserved for a bookable.
mergeAssignedRooms :: [(T.Text, T.Text, S.Set T.Text, S.Set T.Text, BT.Pricing)]
                   -> [(T.Text, T.Text, S.Set T.Text, [T.Text], BT.Pricing)]
mergeAssignedRooms prisInfo =
  let groups = fmap (groupSameBkls prisInfo) (getUniqueBookIds prisInfo)
  in concat $ fmap merge groups 
  where
    toListRooms = fmap (\(_, _, _, assignedRooms, _) -> S.toList assignedRooms)
    merge :: [(T.Text, T.Text, S.Set T.Text, S.Set T.Text, BT.Pricing)]
          -> [(T.Text, T.Text, S.Set T.Text, [T.Text], BT.Pricing)] 
    merge ps = fmap (updateToList ( concat $ toListRooms ps)) ps
      where
        updateToList s (bookId, bookName, roomIds, assignedRooms, pricing)
          = (bookId, bookName, roomIds, s, pricing)

    


-- Given a list of tuples of the form (bookId, bookName, roomIds, a, pricing), return a
-- list of the same type but with no duplicates, where two tuples are the same if they have the
-- same bookId.
removeDuplicateBkls :: [(T.Text, T.Text, S.Set T.Text, a, BT.Pricing)]
                    -> [(T.Text, T.Text, S.Set T.Text, a, BT.Pricing)]
removeDuplicateBkls prisInfo = go prisInfo []
  where
    first (bookId, _, _, _, _) = bookId
    isElem _ [] = False
    isElem p (e:es)
      | first p  == first e = True
      | otherwise = isElem p es
    go [] result = result
    go (e:es) result
      | not (isElem e result) = go es (e:result)
      | otherwise = go es result




-- Given a BasicReservData data type validate it according to email, nameString and description validation.
-- Note that if specialRequests, messageToHost and phoneNumber are not valid, a null will be stored in DB.
-- Also note that if no email is provided, the email passed as argument is the default value.
validateBasicReservData :: RT.BasicReservData
                        -> Maybe T.Text
                        -> ExceptT String IO RT.BasicReservData
validateBasicReservData reservData email' = do
  ExceptT $ return $ validateNameString name
  ExceptT $ return $ validateNameString lName
  (cIn', cOut') <- validateCinCout cIn cOut
  let reservEmail' = maybe email' (\v -> Just v) email >>= optionalField validateEmail  
      sReqs' = sReqs >>= validateDescriptionM
      msg' = msg >>= validateDescriptionM
      phNum' = phNum >>= optionalField validatePhone
  return (RT.BasicReservData propId name lName cIn' cOut' reservEmail' sReqs' msg' phNum')
  where
    (RT.BasicReservData propId name lName cIn cOut email sReqs msg phNum)
      = reservData
    reservEmail = case email of
      Nothing -> email'
      Just v -> Just v
    optionalField validation val = either (\e -> Nothing) (\v -> Just val) (validation val)




{-
  Given a checkIn and a checkOut validate them:
    - checkIn-CheckOut cannot be greater than 30 days.
    - checkIn cannot be greater than checkOut.
    - checkIn cannot be less than current day.
    - A reservation cannot be made with more than 365 days of anticipation.
  Note that the returned (checkIn, checkOut) does not have into account the hour, that is,
  the data type returned is of the form (TM.UTCTime day 0). This is so because our unit of time
  is one day. Every Property will handle manually their particular checkIn and checkOut hours.
-}
validateCinCout :: TM.UTCTime
                -> TM.UTCTime
                -> ExceptT String IO (TM.UTCTime, TM.UTCTime)
validateCinCout cIn cOut = do
  (TM.UTCTime day time) <- liftIO TM.getCurrentTime
  let currentDay = (TM.UTCTime day 0)
      diff1 = TM.diffUTCTime cOut' cIn'
      diff2 = TM.diffUTCTime cIn' currentDay
      maxNights = 2592000 -- 30 nights
      oneNight = 86400
      oneYear = 31536000
  case (cIn' <= cOut', cIn' >= currentDay, diff1 <= maxNights, diff1 >= oneNight, diff2 <= oneYear) of
    (False, _, _, _, _) -> ExceptT $ return greaterCinErr
    (_, False, _, _, _) -> ExceptT $ return cInLessThanCurrent
    (_, _, False, _, _) -> ExceptT $ return moreThan30Nights
    (_, _, _, False, _) -> ExceptT $ return lessThan1Night
    (_, _, _, _, False) -> ExceptT $ return reservMoreThanOneYearBefore
    (True, True, True, True, True) -> return (cIn', cOut')
  where
    cIn' = TM.UTCTime (TM.utctDay cIn) 0
    cOut' = TM.UTCTime (TM.utctDay cOut) 0

   


-- Given a tuple of the form (bookId, _, roomIds, assignedRooms, _) transform it into a tuple (r, calendar)
-- where r are the rooms (number or list) to reserve and calendar is the SCalendar where availability will be checked.
tupleToCalendar :: MonadIO m => TM.UTCTime
                             -> TM.UTCTime
                             -> (T.Text, t1, S.Set T.Text, t2, t3)
                             -> ReaderT MySQL.SqlBackend m (t2, SCT.SCalendar)
tupleToCalendar from to (bookId, _, roomIds, r, _) = do
  maybeCalendar <- createBookableCalendar bookId from to 128 roomIds
  case maybeCalendar of
    Nothing -> return (r, SCT.SCalendar roomIds (SCT.Empty (from, to)))
    Just calendar -> return (r, calendar)



-- Given a checkIn and a checkOut, check the availability of a list of assignedRooms (with duplicates allowed) in a
-- SCalendar. Note that two validations are performed here: first we make sure that the size of the list is the same
-- as the size of the set. In case both sizes are the same, it means that the list had no duplicates. If the first
-- validation is true, then we must make sure that the set without duplicates is available in the calendar.
checkAssignedRoomsAvailability :: TM.UTCTime
                               -> TM.UTCTime
                               -> ([T.Text], SCT.SCalendar)
                               -> Bool
checkAssignedRoomsAvailability from to (assignedRooms, calendar) =
  equalSizes && assignedRoomsAvailability 
  where
    listSize = length assignedRooms
    set = S.fromList assignedRooms
    assignedRoomsAvailability = SC.isReservAvailable (SCT.Reservation set (from,to)) calendar
    equalSizes = listSize == S.size set



{-
 This is a general abstraction to validate the availability of all bookable's related to a reservation, and store that
 reservation together with the information of its pricings.
 The calendar built for each reservation involved starts 32 days before checkIn and has a size of 128 days (around 4 months).
 This is so to have a calendar capable of storing reservations included in an interval (checkIn - 32days, checkOut + 32days),
 which would be the only reservations that would potentially conflict with (checkIn, checkOut) because a reservation cannot
 be greater than 30 nights.
 Thus this function accepts a list of raw pricings (which are the ones to be stored), a list of mereged pricings to validate,
 a function to check the availabilty of those merged pricings in each bookable and a function to store the information about
 that reservation together with its reservedPricings.
 IMPORTANT: The availability check is made with an interval of the form (checkIn, checkOut - 24h).
 This is so because a (checkIn, checkOut) only takes into account the number of nights and the final day, the checkOut,
 is handled particularly by each property admin. Also, take into account the behavior of the function createBookableCalendar.
-}
validateAvailabilityAndStoreReserv :: MonadIO m =>
    TM.UTCTime
 -> TM.UTCTime
 -> RT.BasicReservData
 -> Maybe Integer
 -> [(T.Text, T.Text, S.Set T.Text, a, BT.Pricing)]
 -> [(T.Text, T.Text, S.Set T.Text, b, BT.Pricing)]
 -> (TM.UTCTime -> TM.UTCTime -> (b, SCT.SCalendar) -> Bool)
 -> (Integer -> TM.UTCTime -> TM.UTCTime -> (T.Text, T.Text, S.Set T.Text, a, BT.Pricing) -> ReaderT MySQL.SqlBackend m ())
 -> ReaderT MySQL.SqlBackend m (Either String RT.Reservation) 
validateAvailabilityAndStoreReserv cIn cOut reservInfo userId pricings merged checkAvailability storePricingInfo = do
  let start = TM.addUTCTime (-2764800) cIn     
      end = TM.addUTCTime (2764800) cOut
      lastNight = TM.addUTCTime (-86400) cOut -- check availability up to the last night of the reservation
  calendars <- mapM (tupleToCalendar start end) $ removeDuplicateBkls merged
  if all (checkAvailability cIn lastNight) calendars == False
  then return roomsNotAvailable
  else do
    (reservId, repoReserv) <- RR.storeReservBasicData propId
                                                      userId
                                                      fName
                                                      lName
                                                      cIn
                                                      cOut
                                                      email
                                                      sReqs
                                                      msg
                                                      phNum
    let (RSC.Reservation _ _ submitted expiration abscence _ code _ _ _ _ _ _ _ _ _ _) = repoReserv
        code' = maybe "INVALID_CODE" (\v -> TE.decodeUtf8 v) code
        numDays = round $ (TM.diffUTCTime cOut cIn) / 86400
    mapM (storePricingInfo reservId cIn cOut) pricings
    return $ Right $ RT.Reservation reservId numDays submitted expiration (abscence, cOut) False code' state byAdmin reservInfo
  where
    (RT.BasicReservData propId fName lName cIn cOut email sReqs msg phNum) = reservInfo
    state = maybe RT.Accepted (\v -> RT.Pending) userId
    byAdmin = maybe True (\v -> False) userId

   


-- Transform a repository notification into a notification of the booking domain.
toDomainNotif (notifId, (RSC.HostNotification reservKey msg created subject)) =
  RT.Notification notifId (reservKeyToInt reservKey) msg created (toDomainReservState subject)



{-
Given any Text, return a Dbt.ReservState according to the following rules:
 - "accepted" -> Dbt.Accepted
 - "absent" -> Dbt.Absent
 - "rejected" -> Dbt.Rejected
 - anything else -> Dbt.Pending
-}
setDefaultState :: Maybe T.Text -> DbT.ReservState
setDefaultState st =
  let setRepoState s
        | s == "accepted" = DbT.Accepted
        | s == "absent" = DbT.Absent
        | s == "rejected" = DbT.Rejected
        | otherwise = DbT.Pending
  in maybe DbT.Pending setRepoState st

