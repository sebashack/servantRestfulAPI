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

module Domains.BookingDomain.Bookable.Interpreter ( bookableServer ) where


import Data.ByteString.Char8 (pack)
import Control.Monad.Except
import Network.Wai
import Servant
import Servant.JuicyPixels
import Data.Aeson
import Data.Aeson.Types
import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Domains.BookingDomain.Bookable.API
import Configs.ConfigTypes
import HelperLibs.MySQL.ActionRunner
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad (mapM)
import Servant.JuicyPixels
import HelperLibs.ElasticSearch.ResponseParser
import HelperLibs.Interpreters.Bookable
import HelperLibs.Interpreters.BookingDomain
import qualified Data.ByteString.Base64 as B64
import qualified Database.Persist.MySQL as MySQL
import qualified Data.CountryCodes as CC
import qualified Schemas.SQL.DbTypes as DbT
import qualified Repositories.BookableRepo.Operations as BR
import qualified Repositories.ReservationRepo.Operations as RR (getBklReservedPrisInPeriod_, getBklReservedPrisAfter)
import qualified Repositories.CancellationRepo.Operations as CR (getBklCancInfoInPeriod)
import qualified Domains.BookingDomain.Bookable.DataTypes as BT
import qualified Schemas.SQL.Reservation as RSC
import qualified Data.ByteString.Lazy as LB (toStrict, fromStrict)
import qualified Data.ByteString as SB (length, ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
import qualified Data.Set as S
import qualified HelperLibs.SCalendar.Operations as SC


-- Given a BasicBookableData data type, this function indexes a Bookable, related to a Property, in DB.
-- Note that the user who wants to create a Bookable must match the Property whose ID he's providing.
-- Also note that we are using sets for roomIds so that we avoid repetition.
postBookable :: ConfigMySQL
             -> ConfigES
             -> Maybe T.Text
             -> BT.BasicBookableData
             -> ExceptT ServantErr IO BT.Bookable
postBookable  _ _ Nothing _ = throwError _noToken
postBookable coMysql coEs (Just token) bookData = do
  bookCreation <- liftIO $ runMySQL coMysql $ do
    let propId = BT.propId bookData
    adminCreds <- validateAdminAndMatchProperty coEs token propId
    case adminCreds of
      Left error -> return $ Left error 
      Right (userId, _, _, propType, facs, cCode, region, city) -> do
        case validateBasicBookData bookData of
          Left error -> return $ Left error
          Right validatedData -> do
            let (BT.BasicBookableData propId bklSpecs esDesc enDesc maxOccu roomIds) = validatedData
                (BT.BookableSpecs name roomSize bedType bedNum amenities) = bklSpecs
            jsonRes <- liftIO $ runExceptT $ BR.indexBookable coEs
                                                              propId
                                                              (name, roomSize, bedType, bedNum, amenities)
                                                              esDesc
                                                              enDesc
                                                              maxOccu
                                                              (S.toList roomIds)
                                                              (cCode, region, city) 
                                                              propType
                                                              facs
            case jsonRes of
              Left error -> return indexBookErr 
              Right (Object object) -> return $ maybe bookParsingErr
                                                      (\(String bookId) -> Right (bookId, cCode, validatedData))
                                                      (parseMaybe (object .:) "_id")  
  case getCustomError bookCreation of
    Left error -> throwError error
    Right (bookId, cCode, validatedData) -> return $ BT.Bookable bookId
                                                                (CC.fromText cCode)
                                                                BT.Unlisted
                                                                validatedData
                                                                S.empty




-- Given a bookableId, get that bookable.                                                                
getBookable :: ConfigES
            -> T.Text
            -> ExceptT ServantErr IO BT.Bookable
getBookable config bookId = do
  jsonRes <- liftIO $ runExceptT $ BR.queryBookableById config bookId
  case jsonRes of
    Left _ -> throwError _bookNotFound
    Right (Object object) -> maybe (throwError _bookParsingErr)
                                   (\bookable -> return bookable)
                                   (parseBookable object)
 


-- Given a bookableId change the state of that bookable to Listed if possible.
-- Note that if a Bookable has no images, it cannot be listed.
listBookable :: ConfigMySQL
             -> ConfigES
             -> T.Text
             -> Maybe T.Text
             -> ExceptT ServantErr IO ()
listBookable _ _ _ Nothing = throwError _noToken             
listBookable coMysql coEs bookId (Just token) = do
  update <- liftIO $ runMySQL coMysql $ do
    imgCount <- BR.bookableImageCount bookId
    if imgCount < 1
    then return bklNoImgs
    else updateBookable coEs bookId token True isListable (\v -> Right v) (\a b -> b) BR.updateBookableState
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)



-- Given a bookableId change the state of that bookable to Unlisted if possible.
unlistBookable :: ConfigMySQL
               -> ConfigES
               -> T.Text
               -> Maybe T.Text
               -> ExceptT ServantErr IO ()
unlistBookable _ _ _ Nothing = throwError _noToken             
unlistBookable coMysql coEs bookId (Just token) = do
  update <- liftIO $ runMySQL coMysql $ do
    updateBookable coEs bookId token False isUnlistable (\v -> Right v) (\a b -> b) BR.updateBookableState
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)
    



-- Given a bookableId and a description, update that bookable's spanish description.
updateBookableEsDesc :: ConfigMySQL
                     -> ConfigES
                     -> T.Text
                     -> Maybe T.Text
                     -> T.Text
                     -> ExceptT ServantErr IO ()
updateBookableEsDesc _ _ _ Nothing _ = throwError _noToken
updateBookableEsDesc coMysql coEs bookId (Just token) desc = do
  update <- liftIO $ runMySQL coMysql $ do
    updateBookable coEs bookId token desc (\x -> Right ())
                                          (\x -> (either (\e -> Left e) (\v -> Right v) (validateDescription x)))
                                          (\a b -> b)
                                          BR.updateBookableEsDesc
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)
 




-- Given a bookableId and a description, update that bookable's english description.
updateBookableEnDesc :: ConfigMySQL
                     -> ConfigES
                     -> T.Text
                     -> Maybe T.Text
                     -> T.Text
                     -> ExceptT ServantErr IO ()
updateBookableEnDesc _ _ _ Nothing _ = throwError _noToken
updateBookableEnDesc coMysql coEs bookId (Just token) desc = do
  update <- liftIO $ runMySQL coMysql $ do
    updateBookable coEs bookId token desc (\x -> Right ())
                                          (\x -> (either (\e -> Left e) (\v -> Right v) (validateDescription x)))
                                          (\a b -> b)
                                          BR.updateBookableEnDesc
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)
  




-- Given a BookableSpecs data type and a bookableId, update that bookable's specs. Note that as 
-- roomSize and bedType are optional fields, if invalid values are provided, a null will be stored in DB.
updateBookableSpecs :: ConfigMySQL
                    -> ConfigES
                    -> T.Text
                    -> Maybe T.Text
                    -> BT.BookableSpecs
                    -> ExceptT ServantErr IO ()
updateBookableSpecs _ _ _ Nothing _ = throwError _noToken                    
updateBookableSpecs coMysql coEs bookId (Just token) specs = do
  update <- liftIO $ runMySQL coMysql $ do
    updateBookable coEs bookId token specs (\x -> Right ()) validation (\a b -> b) BR.updateBookableSpecs
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)
  where
    validation bklSpecs = case validateBookSpecs bklSpecs of
      Left error -> Left error
      Right (BT.BookableSpecs name roomSize bedType bedNum amenities) ->
        Right (name, roomSize, bedType, bedNum, amenities)




-- Given a maximum occupancy and a bookableId, update that bookable's maximum occupancy if it is valid. Note that if
-- the given maxOccu is less than the occupancy of any of that bookable's pricings, an error is thrown.
updateBookableMaxOccu :: ConfigMySQL
                      -> ConfigES
                      -> T.Text
                      -> Int
                      -> Maybe T.Text
                      -> ExceptT ServantErr IO ()
updateBookableMaxOccu _ _ _ _ Nothing = throwError _noToken
updateBookableMaxOccu coMysql coEs bookId maxOccu (Just token) = do
  update <- liftIO $ runMySQL coMysql $ do
    updateBookable coEs bookId token maxOccu (checkMaxOccuAndPricings maxOccu)
                                             validateMaxOccupancy
                                             (\a b -> b)
                                             BR.updateBookableMaxOccu
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)                                            
  where
    checkMaxOccuAndPricings n (BT.Bookable bookId cCode status bookData pricings) 
      | all (\pri -> if n >= BT.occupancy (BT.priData pri) then True else False) pricings = Right ()
      | otherwise = maxOccuPriErr

        
        
-- Given a list of roomIds and a bookable's id, add that list into that bookable's set of identifiers, if they're valid. 
-- Note that as the API internally uses sets, if the list of rooms has duplicates, they will be eliminated. Also, 
-- if you try to add a room aidentifier which was already a member of the set, it won't generate a duplicate.
-- The list of room identifiers cannot be greater than 100 elements.
addBookableRoomIds :: ConfigMySQL
                   -> ConfigES
                   -> T.Text
                   -> [T.Text]
                   -> Maybe T.Text
                   -> ExceptT ServantErr IO ()
addBookableRoomIds _ _ _ _ Nothing = throwError _noToken
addBookableRoomIds coMysql coEs bookId roomIds (Just token) = do
  update <- liftIO $ runMySQL coMysql $ do
    updateBookable coEs bookId token (S.fromList roomIds) takeRoomIds
                                                          validateRoomIds
                                                          (\a b -> S.toList $ S.union a b)
                                                          BR.updateBookableRoomIds
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)

  
  
  
-- Given a list of roomIds and a bookable's id, delete that list from that bookable's set of identifiers, if they're valid. 
-- Note that as the API internally uses sets, if the list of rooms has duplicates, they will be eliminated. Also, 
-- if you try to delete a room identifier which wasn't a member of the set it won't have any effect.
-- If the list of roomIds becomes empty, the bookable will be automatically unlisted.
removeBookableRoomIds :: ConfigMySQL
                      -> ConfigES
                      -> T.Text
                      -> [T.Text]
                      -> Maybe T.Text
                      -> ExceptT ServantErr IO ()
removeBookableRoomIds _ _ _ _ Nothing = throwError _noToken
removeBookableRoomIds coMysql coEs bookId roomIds (Just token) = do
  update <- liftIO $ runMySQL coMysql $ do
    roomIds <- updateBookable coEs bookId token (S.fromList roomIds) takeRoomIds
                                                                     validateRoomIds
                                                                     (\a b -> S.toList $ S.difference a b)
                                                                     BR.updateBookableRoomIds
    case roomIds of
      Left error -> return $ Left error
      Right [] -> do
        jsonRes <-liftIO $ runExceptT $ BR.updateBookableState coEs bookId False
        return $ either (\err -> updateBookErr) (\v -> return ()) jsonRes
      _ -> return $ Right ()
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)                                                           

      
  
  
-- Given a PricingData data type, and a bookableId, add new pricing into that bookable's set of pricings, if the data is valid.
-- Note that a Bookable cannot have more than 15 pricings. Also, a pricing cannot have an occupancy greater than the maximum
-- occupancy of the Bookable.
addBookablePricing :: ConfigMySQL
                   -> ConfigES
                   -> T.Text
                   -> Maybe T.Text
                   -> BT.PricingData
                   -> ExceptT ServantErr IO ()
addBookablePricing _ _ _ Nothing _ = throwError _noToken
addBookablePricing coMysql coEs bookId (Just token) priData = do
  update <- liftIO $ runMySQL coMysql $ do
    currentDate <- liftIO TM.getCurrentTime
    let priId = genPricingId bookId currentDate
    updateBookable coEs bookId token priData (takePricings occu)
                                             (validatePricingData priId)
                                             (\set val -> fmap pricingToTuple (S.toList $ S.insert val set))
                                             BR.updateBookablePricings
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)    
  where
    (BT.PricingData occu conds price disc) = priData
    takePricings :: Int -> BT.Bookable -> Either String (S.Set BT.Pricing)
    takePricings n bkl
      | n > maxOccu = priOccuTooLarge
      | n < 1 = badPriOccupancy
      | S.size (BT.pricings bkl) >= 16 = tooManyPricings
      | otherwise = Right $ BT.pricings bkl
      where
        maxOccu = BT.maxOccu (BT.basicData bkl)




-- Given a PrcingData data type, a bookableId and a PricingId, update the information concerning that Pricing, if the 
-- data is valid. Again, a pricing cannot have an occupancy greater than the maximum occupancy of the bookable.           
updateBookablePricing :: ConfigMySQL
                      -> ConfigES
                      -> T.Text
                      -> T.Text
                      -> Maybe T.Text
                      -> BT.PricingData
                      -> ExceptT ServantErr IO ()
updateBookablePricing _ _ _ _ Nothing _ = throwError _noToken
updateBookablePricing coMysql coEs bookId priId (Just token) priData = do
  update <- liftIO $ runMySQL coMysql $ do
    updateBookable coEs bookId token priData (takePricings occu priId)
                                             (validatePricingData priId)
                                             (\set val -> fmap pricingToTuple (S.toList $ S.insert val set))
                                             BR.updateBookablePricings     
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)    
  where
    (BT.PricingData occu conds price disc) = priData
    takePricings :: Int -> T.Text -> BT.Bookable -> Either String (S.Set BT.Pricing)
    takePricings n priId bkl
      | n > maxOccu = priOccuTooLarge
      | n < 1 = badPriOccupancy
      | S.notMember dummyPricing (BT.pricings bkl) = priNotFound
      | otherwise = Right $ BT.pricings bkl
      where
        maxOccu = BT.maxOccu (BT.basicData bkl)
        dummyPricing = (BT.Pricing priId (BT.PricingData 0 [] 0 0))
  




-- Given a bookableId and a pricingId, remove that pricing from that bookable's set of pricings. If the given pricingId
-- does not point to pricing in the that bookable's set, this operation has no effect. Also note that if the list of
-- pricings becomes empty, that bookable will be automatically unlisted.
removeBookablePricing :: ConfigMySQL
                      -> ConfigES
                      -> T.Text
                      -> T.Text
                      -> Maybe T.Text
                      -> ExceptT ServantErr IO ()
removeBookablePricing _ _ _ _ Nothing = throwError _noToken                     
removeBookablePricing coMysql coEs bookId priId (Just token) = do
  update <- liftIO $ runMySQL coMysql $ do
    pricings <- updateBookable coEs bookId token (BT.Pricing priId (BT.PricingData 0 [] 0 0))
                                                 takePricings
                                                 (\v -> Right v)
                                                 (\set val -> fmap pricingToTuple (S.toList $ S.delete val set))
                                                 BR.updateBookablePricings
    case pricings of
      Left error -> return $ Left error
      Right [] -> do
        jsonRes <-liftIO $ runExceptT $ BR.updateBookableState coEs bookId False
        return $ either (\err -> updateBookErr) (\v -> return ()) jsonRes
      _ -> return $ Right ()                                 
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)
  where
    takePricings bkl = Right $ BT.pricings bkl




-- Given a propertyId, get all the bookables related to that property.
getPropBookables :: ConfigES
                 -> T.Text
                 -> ExceptT ServantErr IO [BT.Bookable]
getPropBookables config propId = do
  jsonRes <- liftIO $ runExceptT $ BR.queryBookablesByProp config propId
  case jsonRes of
    Left _ -> throwError _searchBookErr
    Right (Object object) ->
      maybe (throwError _bookParsingErr) (\bookables -> return bookables) (parseBookables object)




-- Given a bookableId and a jpeg DynamicImage, store that image in DB. This function returns the id of
-- that image.
postBookableImg :: ConfigMySQL
                -> ConfigES
                -> T.Text
                -> Maybe T.Text
                -> DynamicImage 
                -> ExceptT ServantErr IO T.Text
postBookableImg _ _ _ Nothing _ = throwError _noToken
postBookableImg coMysql coEs bookId (Just token) (ImageYCbCr8 imgJPEG) = do 
  postImg <- liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdminPropBkl coEs bookId token
    case adminCreds of
      Left error -> return $ Left error
      Right (userId, propId) -> do
        imgCount <- BR.bookableImageCount bookId
        if imgCount > 15 
        then return tooMuchImgs
        else do
          let encodedImg = LB.toStrict (encodeJpegAtQuality 100 imgJPEG)
              numBytes = SB.length encodedImg
          case checkImgBound numBytes of
            LT -> return tooSmallImage
            GT -> return tooLargeImage
            EQ -> do
              imgId <- BR.storeBookableImg bookId encodedImg
              return $ Right imgId
  either (\err -> throwError err) (\v -> return v) (getCustomError postImg)
 


-- Given a bookableId, get all the imageIds related to that bookable.
getBookableImageIds :: ConfigMySQL
                    -> T.Text
                    -> ExceptT ServantErr IO [T.Text]
getBookableImageIds coMysql bookId = do
  imgIds <- liftIO $ runMySQL coMysql $ BR.bookableImageIds bookId
  return imgIds




-- Given a bokableId and an imageId, delete that bookable's image. If the bookable happens to have no images at all,
-- it will be automatically unlisted.
deleteBookableImg :: ConfigMySQL
                  -> ConfigES
                  -> T.Text
                  -> T.Text
                  -> Maybe T.Text
                  -> ExceptT ServantErr IO ()
deleteBookableImg _ _ _ _ Nothing = throwError _noToken
deleteBookableImg coMysql coEs bookId imgId (Just token) = do
  postImg <- liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdminPropBkl coEs bookId token
    case adminCreds of
      Left error -> return $ Left error
      Right (userId, propId) -> do
        BR.deleteBookableImg bookId imgId
        imgCount <- BR.bookableImageCount bookId
        if imgCount > 0 
        then return $ Right ()
        else do
          jsonRes <-liftIO $ runExceptT $ BR.updateBookableState coEs bookId False
          return $ either (\err -> updateBookErr) (\v -> return ()) jsonRes
  either (\err -> throwError err) (\v -> return ()) (getCustomError postImg)      
      

-- Given a bookableId and a period of time (Period from to) this function returns a EconomicReport
-- of all reservedPricings and cancellations of that bookable and whose (checkIn, checkOut) is
-- included in the provided period of time.
getBookableIncome :: ConfigMySQL
                  -> ConfigES
                  -> T.Text
                  -> BT.Period
                  -> Maybe T.Text
                  -> ExceptT ServantErr IO BT.EconomicReport
getBookableIncome _ _ _ _ Nothing = throwError _noToken
getBookableIncome coMysql coEs bookId (BT.Period from to) (Just token) = do
  eReport <- liftIO $ runMySQL coMysql $ do
    eitherBookable <- liftIO $ runExceptT $ queryAndParseBookable coEs bookId
    case eitherBookable of
      Left error -> return $ Left error
      Right bookable -> do
        let propId = BT.propId $ BT.basicData bookable
        adminValidation <- validateAdminAndMatchProperty coEs token propId
        case adminValidation of
          Left error -> return $ Left error
          Right _ -> do
            repoResPris <- RR.getBklReservedPrisInPeriod_ bookId DbT.Accepted (from, to)
            repoCancs <- CR.getBklCancInfoInPeriod bookId (from, to)
            let resPrisInfo =  fmap repoResPriToTuple repoResPris
                cancsInfo = fmap cancInfoToTupleWithList repoCancs
                reservedRooms = sum $ fmap toNumRooms resPrisInfo 
                cancelledRooms = sum $ fmap toNumRooms cancsInfo
                reservationIncome = sum $ fmap getTotalPrice resPrisInfo
                cancellationLoss = sum $ fmap getTotalPrice cancsInfo
            return $ Right (BT.EconomicReport (BT.Period from to) reservedRooms cancelledRooms reservationIncome cancellationLoss)
  either (\err -> throwError err) (\v -> return v) (getCustomError eReport)                  
  where
    repoResPriToTuple (_, (RSC.ReservedPricing _ _ _ cIn cOut _ _ _ price disc text)) =
      (maybe [] (\t -> read (T.unpack t)) text, cIn, cOut, fromIntegral price, disc)
    cancInfoToTupleWithList (text, cIn, cOut, price, disc) =
      (read (T.unpack  text), cIn, cOut, fromIntegral price, disc)
    toNumRooms (ls, _, _, _, _) = fromIntegral $ length ls
    -- Note that we have to round the total result because we represent currencies as Integers.
    getTotalPrice :: ([T.Text], TM.UTCTime, TM.UTCTime, Integer, Int) -> Integer
    getTotalPrice (ls, cIn, cOut, price, disc) =
      let numNights = round $ (TM.diffUTCTime cOut cIn) / 86400
      in round $ (fromIntegral numNights) *
                 (fromIntegral $ length ls) *
                 (fromIntegral price) *
                 ((fromIntegral $ 100 - disc) / 100)
     


-- Given a bookableId and a discount, apply that discount to all the pricings belonging to that bookable, if it is valid.
applyBookableDiscount :: ConfigMySQL
                      -> ConfigES
                      -> T.Text
                      -> Int
                      -> Maybe T.Text
                      -> ExceptT ServantErr IO ()
applyBookableDiscount _ _ _ _ Nothing = throwError _noToken                      
applyBookableDiscount coMysql coEs bookId disc (Just token) = do
  update <- liftIO $ runMySQL coMysql $ do
    updateBookable coEs bookId token disc takePricings
                                          validateDiscount
                                          (\set d -> fmap pricingToTuple $ fmap (applyDisc d) (S.toList set))
                                          BR.updateBookablePricings
  either (\err -> throwError err) (\v -> return ()) (getCustomError update)      
  where
    takePricings bkl = Right $ BT.pricings bkl
    validateDiscount d
      | d < 0 || d > 100 = badPriDisc
      | otherwise = Right d
    applyDisc d (BT.Pricing priId priData) = (BT.Pricing priId (priData { BT.roomDiscount = d }))  




-- Given a propertyId and a CheckInOut data type, this function returns a list of
-- searchResults of the availability of that property's bookables.
getPropBookableAvailability :: ConfigMySQL
                            -> ConfigES
                            -> T.Text
                            -> BT.CheckInOut
                            -> ExceptT ServantErr IO [BT.SearchResultWithImgIds]
getPropBookableAvailability coMysql coEs propId (BT.CheckInOut cIn cOut) = do
  search <- liftIO $ runMySQL coMysql $ do
    jsonRes <- liftIO $ runExceptT $ BR.queryBookablesByProp coEs propId
    case jsonRes of
      Left _ -> return searchBookErr
      Right (Object object) -> do
        -- We are using here getSearchResults with numRooms == 0 to get results from
        -- all bookables and with size == 1000 so that we get a list of all bookables.
        eitherResults <- getSearchResults coEs object cIn cOut 0 0 1000
        case eitherResults of
          Left error -> return $ Left error
          Right results -> do
            resultsWithImgs <- mapM getSearchResultImgIds results
            return $ Right resultsWithImgs
  either (\err -> throwError err) (\v -> return v) (getCustomError search)      



-- Given a BasicSearch data type this function returns a list of searchResults which
-- match the given BasicSearch. 
makeBasicSearch :: ConfigMySQL
                -> ConfigES
                -> Maybe Int
                -> Maybe Int
                -> BT.BasicSearch
                -> ExceptT ServantErr IO [BT.SearchResult]
makeBasicSearch coMysql coEs from size basicSearch = do
  search <- liftIO $ runMySQL coMysql $ do
    -- We are only concerned with the first 600 bookables found by ElasticSearch.
    jsonRes <- liftIO $ runExceptT $ BR.basicBookableQuery coEs cCode' guestNum' locStr' 0 600
    case jsonRes of
      Left _ -> return searchBookErr
      Right (Object object) -> do
        getSearchResults coEs object cIn cOut numRooms' from' size'
  either (\err -> throwError err) (\v -> return v) (getCustomError search)                  
  where
    (BT.BasicSearch cCode guestNum locStr (BT.CheckInOut cIn cOut) numRooms) = basicSearch
    from' = maybe 0 (setBound 0) from
    size' = maybe 15 (setBound 15) size
    guestNum' = setBound 1 guestNum
    numRooms' = setBound 1 numRooms
    locStr' = maybe "" id locStr
    cCode' = CC.toText cCode




-- Given a AdvancedSearch data type this function returns a list of searchResults which
-- match the given AdvancedSearch. 
makeAdvancedSearch :: ConfigMySQL
                   -> ConfigES
                   -> Maybe Int
                   -> Maybe Int
                   -> BT.AdvancedSearch
                   -> ExceptT ServantErr IO [BT.SearchResult] 
makeAdvancedSearch coMysql coEs from size advancedSearch = do
  search <- liftIO $ runMySQL coMysql $ do
    -- We are only concerned with the first 600 bookables found by ElasticSearch.
    jsonRes <- liftIO $ runExceptT $ BR.advancedBookableQuery coEs cCode' guestNum'
                                                              locStr' propType facs
                                                              amens numBeds priRange'
                                                              disc' 0 600
    case jsonRes of
      Left _ -> return searchBookErr
      Right (Object object) -> do
        getSearchResults coEs object cIn cOut numRooms' from' size'
  either (\err -> throwError err) (\v -> return v) (getCustomError search)  
  where
    (BT.AdvancedSearch basicSearch priRange disc propType facs amens numBeds) = advancedSearch 
    (BT.BasicSearch cCode guestNum locStr (BT.CheckInOut cIn cOut) numRooms) = basicSearch
    from' = maybe 0 (setBound 0) from
    size' = maybe 15 (setBound 15) size
    guestNum' = setBound 1 guestNum
    numRooms' = setBound 1 numRooms
    locStr' = maybe "" id locStr
    cCode' = CC.toText cCode
    numBeds' = fmap (setBound 1) numBeds
    disc' = fmap (\v -> if v >= 0 && v <= 100 then v else 0) disc
    priRange' = do
      (BT.PriceRange l u) <- priRange
      if l < u && l >= 0
      then Just (l, u)
      else Nothing



-- Given a bookableId and a Period (from, to) data type this function returns a report about
-- the availability of that bookable in that period of time.
getBookablePeriodReport :: ConfigMySQL
                        -> ConfigES
                        -> T.Text
                        -> BT.Period
                        -> Maybe T.Text
                        -> ExceptT ServantErr IO BT.Report
getBookablePeriodReport _ _ _ _ Nothing = throwError _noToken 
getBookablePeriodReport coMysql coEs bookId (BT.Period from to) (Just token) = do
  report <- liftIO $ runMySQL coMysql $ do
    eitherBookable <- liftIO $ runExceptT $ queryAndParseBookable coEs bookId
    case eitherBookable of
      Left error -> return $ Left error
      Right bookable -> do
        let propId = BT.propId $ BT.basicData bookable
            start = TM.addUTCTime (-2764800) from     
            end = TM.addUTCTime (2764800) to
            lastNight = TM.addUTCTime (-86400) to -- create report up to the last night of the period
            roomIds = BT.roomIds $ BT.basicData bookable
        adminValidation <- validateAdminAndMatchProperty coEs token propId    
        maybeCalendar <- createBookableCalendar bookId start end 128 roomIds
        case (adminValidation, maybeCalendar) of
          (Left error, _) -> return $ Left error
          (_, Nothing) -> return calendarCreationErr
          (Right _, Just calendar) -> do
            let maybeReport = fmap toDomainReport (SC.periodReport (from, lastNight) calendar)
            return $ maybe reportCreationErr (\v -> Right v) maybeReport 
  either (\err -> throwError err) (\v -> return v) (getCustomError report)          



-- Given a propertyId and a Period (from, to) data type this function returns a list of
-- periodReports about the availability of that property's bookables in that period of time.
getPropertyPeriodReport :: ConfigMySQL
                        -> ConfigES
                        -> T.Text
                        -> BT.Period
                        -> Maybe T.Text
                        -> ExceptT ServantErr IO [BT.PeriodReport]
getPropertyPeriodReport _ _ _ _ Nothing = throwError _noToken 
getPropertyPeriodReport coMysql coEs propId (BT.Period from to) (Just token) = do
  search <- liftIO $ runMySQL coMysql $ do
    eitherBookables <- liftIO $ runExceptT $ queryAndParsePropBookables coEs propId
    adminValidation <- validateAdminAndMatchProperty coEs token propId
    case (adminValidation, eitherBookables) of
      (Left error, _) -> return $ Left error
      (_, Left error) -> return $ Left error
      (Right _, Right bookables) -> do
        let start = TM.addUTCTime (-2764800) from
            end = TM.addUTCTime (2764800) to
            lastNight = TM.addUTCTime (-86400) to -- create reports up to the last night of the period
        reports <- mapM (createBookPeriodReport (start, end) (from, lastNight)) bookables
        return $ sequence reports
  either (\err -> throwError err) (\v -> return v) (getCustomError search)
  where
    createBookPeriodReport (start, end) (up, lo) bkl = do
      let roomIds = BT.roomIds $ BT.basicData bkl
          bookId = BT.bklId bkl
      maybeCalendar <- createBookableCalendar bookId start end 128 roomIds
      case maybeCalendar of
        Nothing -> return calendarCreationErr
        Just calendar -> do
          let maybeReport = fmap toDomainReport (SC.periodReport (up, lo) calendar)
          return $ maybe (reportCreationErr) (\r -> Right (BT.PeriodReport bkl r)) maybeReport


deleteBookable :: ConfigMySQL
               -> ConfigES
               -> T.Text
               -> BT.UserCredentials
               -> Maybe T.Text
               -> ExceptT ServantErr IO ()
deleteBookable _ _ _ _ Nothing = throwError _noToken
deleteBookable coMysql coEs bookId (BT.UserCredentials identifier password) (Just token) = do
  deletion <- liftIO $ runMySQL coMysql $ do
    userValidation <- matchUserTokenAndCredentials identifier password token
    adminValidation <- validateAdminAndMatchBookable coEs token bookId
    case (userValidation, adminValidation) of
      (Left error, _) -> return $ Left error
      (_, Left error) -> return $ Left error
      (Right _, Right _) -> do
        (TM.UTCTime day time) <- liftIO $ TM.getCurrentTime
        resPris <- RR.getBklReservedPrisAfter bookId DbT.Accepted (TM.UTCTime day 0)
        if null resPris
        then do
          jsonRes <- liftIO $ runExceptT $ BR.deleteBookable coEs bookId
          either (\err -> return  bklDeletionErr)(\v -> return $ Right ()) jsonRes
        else return cannotDeleteBkl
  either (\err -> throwError err) (\v -> return v) (getCustomError deletion) 


  
bookableServer :: ConfigMySQL -> ConfigES -> Server BookableAPI
bookableServer coMysql coEs = (getBookable coEs)
                         :<|> (postBookable coMysql coEs)
                         :<|> (listBookable coMysql coEs)
                         :<|> (unlistBookable coMysql coEs)
                         :<|> (deleteBookable coMysql coEs)     
                         :<|> (updateBookableEsDesc coMysql coEs)
                         :<|> (updateBookableEnDesc coMysql coEs)
                         :<|> (updateBookableSpecs coMysql coEs)
                         :<|> (updateBookableMaxOccu coMysql coEs)
                         :<|> (addBookableRoomIds coMysql coEs)
                         :<|> (removeBookableRoomIds coMysql coEs)
                         :<|> (addBookablePricing coMysql coEs)
                         :<|> (updateBookablePricing coMysql coEs) 
                         :<|> (removeBookablePricing coMysql coEs)
                         :<|> (getPropBookables coEs)
                         :<|> (getPropBookableAvailability coMysql coEs)
                         :<|> (makeBasicSearch coMysql coEs)
                         :<|> (makeAdvancedSearch coMysql coEs)
                         :<|> (postBookableImg coMysql coEs)
                         :<|> (getBookableImageIds coMysql)
                         :<|> (deleteBookableImg coMysql coEs)
                         :<|> (getBookablePeriodReport coMysql coEs)
                         :<|> (getBookableIncome coMysql coEs)
                         :<|> (applyBookableDiscount coMysql coEs)
                         :<|> (getPropertyPeriodReport coMysql coEs) 
