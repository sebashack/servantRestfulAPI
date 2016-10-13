{-# LANGUAGE OverloadedStrings #-}


module HelperLibs.Interpreters.Bookable where

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
import Data.Monoid
import Data.Maybe (catMaybes)
import HelperLibs.MySQL.ActionRunner
import qualified Data.CountryCodes as CC
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Domains.BookingDomain.Bookable.DataTypes as BT
import qualified Domains.BookingDomain.Property.DataTypes as PT
import qualified Repositories.BookableRepo.Operations as BR
import qualified Repositories.ReviewRepo.Operations as RvT (getPropertyScores)
import qualified Schemas.SQL.DbTypes as DbT
import qualified Data.Map.Strict as Map
import qualified Text.Email.Validate as EV
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
import qualified Database.Persist.MySQL as MySQL
import qualified Repositories.UserRepo.Operations as UR (getAdminValidationData)
import qualified Data.ByteString as SB (ByteString)
import qualified Data.Set as S
import qualified HelperLibs.SCalendar.DataTypes as SCT
import qualified HelperLibs.SCalendar.Operations as SC


-- Given a SCT.Report (The Report data type of the SCalendar library) transform it into a 
-- Report data tpe of the Bookable domain.
toDomainReport :: SCT.Report -> BT.Report
toDomainReport (SCT.Report (from, to) total reserved remaining) =
  BT.Report total reserved remaining



-- A valid maxOccupancy must be greater than 0 but less than or equal to 15.
validateMaxOccupancy :: Int -> Either String Int
validateMaxOccupancy occu
  | occu < 1 = badOccupancy
  | occu > 15 = badOccupancy
  | otherwise = Right occu



-- Validate a list of roomIds: A valid list of roomIds must not be greater than 100 elements,
-- and each element must be less than 15 characters long. Empty Strings are not allowed. 
-- A list of roomIds can be empty.
validateRoomIds :: S.Set T.Text -> Either String (S.Set T.Text)
validateRoomIds ids
  | all isValidRoomId ids = Right ids
  | S.size ids > 100 = badRoomIds
  | otherwise = badRoomIds
  where
    isValidRoomId roomId
      | T.length roomId < 1 = False
      | T.length roomId > 15 = False
      | otherwise = True


-- Bookable names must be greater than 4 characters and less than 31.
validateBookName :: T.Text -> Either String T.Text
validateBookName name
  | numChars < 4 = badBookName
  | numChars > 31 = badBookName
  | not (T.any isAlphaNum name) = badBookName
  | otherwise = Right name
  where
    numChars = T.length name 



-- A room size must be greater than 3 characters and less than 10.
validateRoomSize :: T.Text -> Maybe T.Text
validateRoomSize rSize
  | numChars < 3 = Nothing
  | numChars > 10 = Nothing
  | not (T.any isDigit rSize) = Nothing
  | otherwise = Just rSize
  where
    numChars = T.length rSize 



-- Bed types must be greater than 4 characters and less than 21.
validateBedType :: T.Text -> Maybe T.Text
validateBedType bedType
  | numChars < 4 = Nothing
  | numChars > 21 = Nothing
  | not (T.any isAlphaNum bedType) = Nothing
  | otherwise = Just bedType
  where
    numChars = T.length bedType 



-- A valid number of beds must be greater than 0 but less than or equal to 15.
validateBedNum :: Int -> Either String Int
validateBedNum bedNum
  | bedNum < 1 = badBedNum
  | bedNum > 15 = badBedNum
  | otherwise = Right bedNum



-- Validate a list of amenities: A valid list of amenities must not be greater than 50 elements, and each element must be
-- less than 50 characters long. Empty Strings are not allowed. 
validateAmenities :: [T.Text] -> Either String [T.Text]
validateAmenities amens
  | length amens > 50 = badAmenities
  | all isValidAmen amens = Right amens
  | otherwise = badAmenities
  where
    isValidAmen amen
      | T.length amen < 1 = False
      | T.length amen > 50 = False
      | otherwise = True



-- Given a BookableSpecs data type validate it. Note that in case that roomSize or
-- bedType is not valid a Nothing is returned inside BookableSpecs.
validateBookSpecs :: BT.BookableSpecs -> Either String BT.BookableSpecs
validateBookSpecs bklSpecs = do
  -- Optional fields
  let roomSize' = roomSize >>= validateRoomSize
      bedType' = bedType >>= validateBedType
  -- Obligatory fields
  validateBookName name
  validateBedNum bedNum
  validateAmenities amenities
  return $ BT.BookableSpecs name roomSize' bedType' bedNum amenities
  where
    (BT.BookableSpecs name roomSize bedType bedNum amenities) = bklSpecs

    

-- Validate the basic information to create a bookable. Note that in case that esDesc or
-- enDesc is not valid a Nothing is returned inside BasicBookableData. 
validateBasicBookData :: BT.BasicBookableData -> Either String BT.BasicBookableData
validateBasicBookData bookData = do
  -- Obligatory fields
  validateMaxOccupancy maxOccu
  validateRoomIds roomIds
  validateBookSpecs bklSpecs
  -- Optional fields
  let esDesc' = esDesc >>= validateDescriptionM
      enDesc' = enDesc >>= validateDescriptionM  
  return $ BT.BasicBookableData propId bklSpecs esDesc' enDesc' maxOccu roomIds
  where
    (BT.BasicBookableData propId bklSpecs esDesc enDesc maxOccu roomIds) = bookData
 

-- Given a Bookable get its roomIds.
takeRoomIds :: BT.Bookable -> Either String (S.Set T.Text)
takeRoomIds bkl = Right $ BT.roomIds (BT.basicData bkl)




-- Validate a PricingData: A valid PricingData has an occupancy greater than 0, has a list of non-empty conditions which is less than
-- 20 elements and every element is less than 350 characters, has a price greater than 0, and has a discount which is an integer
-- between 0 and 100.
validatePricingData :: T.Text
                    -> BT.PricingData
                    -> Either String BT.Pricing
validatePricingData _ (BT.PricingData occu conds price disc)
  | occu < 1 = badPriOccupancy
  | not $ all isValidCond conds = badPriConds
  | length conds > 20 = badPriConds
  | price < 1 = badPriPrice
  | disc < 0 || disc > 100 = badPriDisc
  where
    isValidCond cond 
      | numChars < 3  = False
      | numChars > 350 = False
      | otherwise = True
      where
        numChars = T.length cond
validatePricingData priId priData = Right $ (BT.Pricing priId priData) 

  
-- Given a bookableId and a UTCTime, generate a pricingId.
genPricingId :: T.Text -> TM.UTCTime -> T.Text  
genPricingId bookId utc = bookId <> timeStamp
  where    
    (TM.UTCTime date time) = utc
    (year, monthDay) = splitAt 4 (show date)
    (month, day') = splitAt 2 (tail monthDay)
    day = tail day'
    hour = show $ round (time * 1000000)
    timeStamp = T.pack $ year ++ month ++ day ++ hour



-- Transform a BT.Pricing into a tuple.
pricingToTuple :: BT.Pricing -> (T.Text, Int, [T.Text], Integer, Int)
pricingToTuple (BT.Pricing priId priData) = (priId, occu, conds, price, disc)
  where
    (BT.PricingData occu conds price disc) = priData


    

-- Check if a Bookable can be listed: A Bookable can be listed if it has pricings, if its status is Unlisted, if it has
-- either a description in english or in spanish, if it has been assigned some roomIds.
isListable (BT.Bookable bookId _ status bookData pricings) 
  | status == BT.Listed = alreadyListed
  | null roomIds = bklNoRooms
  | esDesc == Nothing && enDesc == Nothing  = bklNoDescs
  | null pricings = emptyPris
  | otherwise = Right ()
  where
    (BT.BasicBookableData propId bklSpecs esDesc enDesc maxOccu roomIds) = bookData
    


-- Check if a Bookable can be unlisted: A Bookable can be unlisted if is state is listed.
isUnlistable :: BT.Bookable -> Either String ()
isUnlistable (BT.Bookable bookId _ status bookData pricings) 
  | status == BT.Listed = Right ()
  | otherwise = alreadyUnlisted


{-
General abstraction to update bookables, it receives:

 - A bookableId
 - A token
 - A value to be updated.
 - A function which validates if the bookable can be updated.
 - A function which validates if the value to be updated is correct.
 - A binary operation which operates with the return values of the previous functions.
 - A function which updates the resulting value in DB.

This function returns the result of the binary operation if no errors have occured.
-}
updateBookable :: MonadIO m => ConfigES
                            -> T.Text
                            -> T.Text
                            -> t
                            -> (BT.Bookable -> Either String t1)
                            -> (t -> Either String t2)
                            -> (t1 -> t2 -> b)
                            -> (ConfigES -> T.Text -> b -> ExceptT a IO b1)
                            -> ReaderT MySQL.SqlBackend m (Either String b)
updateBookable coEs bookId token value validation1 validation2 biOp updateFunc  = do
  eitherBookable <- liftIO $ runExceptT $ queryAndParseBookable coEs bookId
  case eitherBookable of
    Left error -> return $ Left error
    Right bookable -> do
      case (validation1 bookable, validation2 value) of
        (Left error, _) -> return $ Left error
        (_, Left error) -> return $ Left error
        (Right bookValue, Right validatedValue) -> do
          let propId = BT.propId $ BT.basicData bookable
          adminCreds <- validateAdminAndMatchProperty coEs token propId
          case adminCreds of
            Left error -> return $ Left error
            Right _ -> do
              let result = biOp bookValue validatedValue
              jsonRes <- liftIO $ runExceptT $ updateFunc coEs bookId result
              return $ either (\err -> updateBookErr) (\val -> Right result) jsonRes
                  



-- General validation procedure to Validate an admin and to match an admin, a property and a bookable.
validateAdminPropBkl :: MonadIO m => ConfigES
                                  -> T.Text
                                  -> T.Text
                                  -> ReaderT MySQL.SqlBackend m (Either String (Integer, T.Text))
validateAdminPropBkl coEs bookId token = do
  eitherBookable <- liftIO $ runExceptT $ queryAndParseBookable coEs bookId 
  case eitherBookable of
    Left error -> return $ Left error
    Right bookable -> do
      let propId = BT.propId $ BT.basicData bookable
      adminCreds <- validateAdminAndMatchProperty coEs token propId
      return $ either (\err -> Left err)
                      (\(userId, _, _, _, _, _, _, _) -> Right (userId, propId))
                      adminCreds


-- Given a interval (from, to) to create a calendar, and an interval (cIn, cOut) to check
-- availability of a bookable in that period this function checks if a given number of rooms
-- is available.      
-- This function computed the meanScore of each bookable's property with the first 1500 most recent scores.
-- IMPORTANT: take into account the behavior of createBookableCalendar  
checkBookableAvailability :: MonadIO m => ConfigES
                                       -> (TM.UTCTime, TM.UTCTime)
                                       -> (TM.UTCTime, TM.UTCTime)
                                       -> Int
                                       -> Int
                                       -> BT.Bookable
                                       -> ReaderT MySQL.SqlBackend m (Maybe BT.SearchResult) 
checkBookableAvailability coEs (from, to) (cIn, cOut) numDays numRooms bookable = do
  let propId = BT.propId $ BT.basicData bookable
      bookId = BT.bklId bookable
      roomIds = BT.roomIds $ BT.basicData bookable
      lastNight = TM.addUTCTime (-86400) cOut -- check availability up to the last night of the reservation
  maybeCalendar <- createBookableCalendar bookId from to numDays roomIds
  maybeProperty <- liftIO $ queryAndParseProperty coEs propId
  reviewScores <- RvT.getPropertyScores propId 1500 -- get the 1500 most recent scores.
  case (maybeCalendar, maybeProperty) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just calendar, Just property) -> do
      let maybeReport = SC.periodReport (cIn, lastNight) calendar
      case maybeReport of
        Nothing -> return Nothing
        Just (SCT.Report _ total reserved remaining) -> do
          let propImg = PT.mainImgId property
              propName = PT.name $ PT.propData property
              propType =  PT.propType $ PT.propData property
              availableRooms = S.size remaining
              scoresLength = fromIntegral $ length reviewScores
              scoreMean = if scoresLength == 0 then 0 else (sum reviewScores) / scoresLength
              roundedMean = (fromInteger $ round $ scoreMean * 10) / 10.0
          if availableRooms >= numRooms
          then return $ Just (BT.SearchResult bookable availableRooms roundedMean propName propType propImg)
          else return Nothing



-- Given an Object data type which is supposed to be a parsable ElasticSearch response with a list of bookables,
-- this function checks the availability of a given number of rooms in a that list of bookables for a
-- (checkIn, checkOut), and returns a list of searchResults of the bookables which have availability.
getSearchResults :: MonadIO m => ConfigES
                              -> Object
                              -> TM.UTCTime
                              -> TM.UTCTime
                              -> Int
                              -> Int
                              -> Int
                              -> ReaderT MySQL.SqlBackend m (Either String [BT.SearchResult])
getSearchResults coEs object cIn cOut numRooms from size = do
  case parseBookables object of
    Nothing -> return bookParsingErr
    Just bookables -> do
      let start = TM.addUTCTime (-2764800) cIn     
          end = TM.addUTCTime (2764800) cOut
      maybeResults <- mapM (checkBookableAvailability coEs (start, end) (cIn, cOut) 128 numRooms) bookables
      let results = take size $ drop from (catMaybes maybeResults)
      return $ Right results
      

      
getSearchResultImgIds :: MonadIO m => BT.SearchResult
                                   -> ReaderT MySQL.SqlBackend m BT.SearchResultWithImgIds
getSearchResultImgIds sResult = do
  let bookId = BT.bklId $ BT.bookable $ sResult
  imgIds <- BR.bookableImageIds bookId
  return $ BT.SearchResultWithImgIds sResult imgIds
      
      
      
      
      
      
