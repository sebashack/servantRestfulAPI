{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.Interpreters.BookingDomain where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Servant
import Control.Monad.Except
import Configs.ConfigTypes
import HelperLibs.ElasticSearch.ResponseParser
import Crypto.PasswordStore
import qualified Data.Set as S
import qualified HelperLibs.SCalendar.Operations as SC
import qualified HelperLibs.SCalendar.DataTypes as SCT
import qualified Data.Vector as V
import qualified Data.CountryCodes as CC
import qualified Data.Text as T
import qualified Schemas.SQL.DbTypes as DbT
import qualified Domains.UserDomain.User.DataTypes as UT
import qualified Data.Map.Strict as Map
import qualified Web.JWT as JWT
import qualified Text.Email.Validate as EV
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
import qualified Schemas.SQL.Reservation as RSC
import qualified Domains.BookingDomain.Property.DataTypes as PT
import qualified Domains.BookingDomain.Bookable.DataTypes as BT 
import qualified Domains.BookingDomain.Reservation.DataTypes as RT
import qualified Schemas.SQL.User as USC
import qualified Database.Persist.MySQL as MySQL
import qualified Repositories.ReservationRepo.Operations as RR (getReservById, getBklReservedPrisInPeriod_)
import qualified Repositories.UserRepo.Operations as UR (getAdminValidationData, getUserAuthData)
import qualified Repositories.PropertyRepo.Operations as PR (queryPropertyById)
import qualified Repositories.BookableRepo.Operations as BR (queryBookableById, queryBookablesByProp)
import qualified Data.ByteString as SB (ByteString)


-- Convert a MySQL user backend key into an integer.
userKeyToInt key = toInteger $ MySQL.unSqlBackendKey (USC.unUserKey key)

-- Covert a MySQL reservation backend key into an integer
reservKeyToInt key = toInteger $ MySQL.unSqlBackendKey (RSC.unReservationKey key)


-- Passwords must be alphanumeric characters. At least one character must be a number.
-- Also, a password must have at least 7 characters but less than 20 characters.
validatePassword :: T.Text -> Either String T.Text
validatePassword password
  | numChars  < 7 = badPass
  | numChars  > 21 = badPass
  | not (T.all isAlphaNum password) = badPass
  | not (T.any isDigit password) = badPass
  | otherwise = Right password
  where
    numChars = T.length password

                       
-- Given a token with the "Bearer" scheme attached to it, return the userdId, profName and token without "Bearer".
getTokenUserIdAndProfName :: T.Text -> Either String (Integer, T.Text, T.Text)
getTokenUserIdAndProfName auth = do
  case stripBearerScheme auth of
    Nothing -> malformedToken
    Just token ->      
      case JWT.decode token of
        Nothing -> malformedToken
        Just credential -> 
          case (lookUpInToken "userId" credential, lookUpInToken "profName" credential) of
            (Just (Number userId), Just (String profName)) -> Right $ (round userId, profName, token)
            _ -> malformedToken
  where
    lookUpInToken attr claims = Map.lookup attr $ JWT.unregisteredClaims (JWT.claims claims)
    stripBearerScheme :: T.Text -> Maybe T.Text
    stripBearerScheme text = do
      suffix <- T.stripPrefix "Bearer" text
      return $ T.stripStart suffix


-- Given a Text with assigned rooms parse it into a Set of assigned rooms.
parseRooms :: Maybe T.Text -> S.Set T.Text
parseRooms rooms = case rooms of
  Nothing -> S.empty
  Just text -> (S.fromList . read . T.unpack) text


-- Transform a repository reservedPricing into a pricingInfo.  
toDomainResPri (resPriId, resPri) =
  let (RSC.ReservedPricing rId bookId bookName cIn cOut conds occu numRooms price disc rooms) = resPri
      bookId' = TE.decodeUtf8 bookId
      rooms' = parseRooms rooms
      conds' = (S.fromList . read . T.unpack) conds 
  in (RT.PricingInfo resPriId bookId' bookName conds' occu cIn cOut numRooms (fromIntegral price) rooms' disc)



-- Transform a repository reservation into a reservation of the booking domain.
toDomainReserv :: (Integer, RSC.Reservation) -> RT.Reservation
toDomainReserv (reservId, reserv) =
  let (RSC.Reservation byAdmin userKey submitted exp absence
                       arrived code state propId fName lName
                       email cIn cOut sReqs msg phNum) = reserv                          
      propId' = (TE.decodeUtf8 propId)
      state' = toDomainReservState state
      code' = maybe "IVALID_CODE" (\v -> v) (fmap TE.decodeUtf8 code)
      basicData = RT.BasicReservData propId' lName fName cIn cOut email sReqs msg phNum
      numDays = round $ (TM.diffUTCTime cOut cIn) / 86400
  in RT.Reservation reservId numDays submitted exp (absence, cOut) arrived code' state' byAdmin basicData



-- Given a Reservation sate from repository convert it into its Domain equivalent.
toDomainReservState s = case s of
  DbT.Pending -> RT.Pending
  DbT.Accepted -> RT.Accepted
  DbT.Rejected -> RT.Rejected
  DbT.Absent -> RT.Absent



-- Given an authentication token, validate a user.
validateUser :: MonadIO m => T.Text
                          -> ReaderT MySQL.SqlBackend m (Either String (Integer, T.Text, T.Text, SB.ByteString, T.Text))
validateUser auth = do 
  case getTokenUserIdAndProfName auth of
    Left error -> return $ Left error
    Right (userId, _, token) -> do
      credentials <- UR.getUserAuthData userId
      case credentials of
        (Nothing, Nothing, Nothing, Nothing, Nothing) -> return userNotFound
        (_, _, _, Nothing, _) -> return noSecret
        (_, _, _, _, Nothing) -> return expiredToken
        (Just profName, Just mainEmail, Just hashedPass, Just secret, Just exp) -> do
          currentDate <- liftIO TM.getCurrentTime
          case exp <= currentDate of
            True -> return expiredToken
            False -> do
              case JWT.decodeAndVerifySignature (JWT.secret secret) token of
                Nothing -> return invalidSignature
                Just _ -> return $ Right (userId, profName, mainEmail, hashedPass, secret)



-- Given an authentication token, validate a property admin according to token expiration, valid token signature and if that
-- admin is blocked or not. This function returns the userId and the number of properties that admin is allowed to create. 
validateAdmin :: MonadIO m => T.Text
                           -> ReaderT MySQL.SqlBackend m (Either String (Integer, Int))
validateAdmin auth = do
  case getTokenUserIdAndProfName auth of
    Left error -> return $ Left error
    Right (userId, profName, token) -> do
      credentials <- UR.getAdminValidationData userId
      case credentials of
        (Nothing, Nothing, Nothing, Nothing) -> return userNotFound
        (_, _, Nothing, _) -> return noSecret
        (_, _, _,Nothing) -> return expiredToken
        (Just True, _, _, _) -> return blockedAdmin
        (Just False, Just canCreate, Just secret, Just exp) -> do
          currentDate <- liftIO TM.getCurrentTime
          case exp <= currentDate of
            True -> return expiredToken
            False -> do
              let verification = (JWT.decodeAndVerifySignature (JWT.secret secret) token)
              return $ maybe invalidSignature (\val -> Right (userId, canCreate)) verification
                             

              
matchUserTokenAndCredentials :: MonadIO m => T.Text
                                          -> T.Text
                                          -> T.Text
                                          -> ReaderT MySQL.SqlBackend m (Either String Integer)
matchUserTokenAndCredentials identifier password token = do
  credentials <- validateUser token
  case credentials of
    Left error -> return $ Left error
    Right (userId, profName, mainEmail, hashedPass, secret) -> do
      let profNameOrEmailMatches = profName == identifier || mainEmail == identifier
          passwordMatches = verifyPassword (TE.encodeUtf8 password) hashedPass
      case (profNameOrEmailMatches, passwordMatches) of
        (False, _) -> return profNameOrEmailMismatch
        (_, False) -> return passMismatch 
        (True, True) -> return $ Right userId


    
   
-- See if a given userdId Matches a Property. This function returns some Property fields which are convinient
-- for other functions: mainImgId, propFacilities, propCountryCode, propRegion, propCity and propType.
matchPropertyAdmin :: ConfigES
                   -> Integer
                   -> T.Text
                   -> IO (Either String (Maybe T.Text, T.Text, [T.Text], T.Text, T.Text, T.Text))
matchPropertyAdmin config userId propId = do
  jsonRes <- runExceptT $ PR.queryPropertyById config propId
  case jsonRes of
    Left error -> return propNotFound
    Right (Object object) ->
      case parseProperty object of
        Nothing -> return propParsingErr
        Just property -> do
          let adminId = parseAdminId object
              (PT.Property _ basicData _ _ _ facs _ _ imgId) = property
              (PT.BasicPropData _ propType location) = basicData
              (PT.Location cCode region city _ _) = location
          case fmap (\val -> val == userId) adminId of
            Nothing -> return propParsingErr
            Just False -> return propAdminMismatch
            Just True -> return $ Right (imgId, propType, facs, CC.toText cCode, region, city)
  where
    parseAdminId :: Object -> Maybe Integer
    parseAdminId obj = do
      _source <- parseMaybe (obj .:) "_source" :: Maybe Object
      (Number uId) <- parseMaybe (_source .:) "admin_id"
      return $ round uId



-- This function combines the validations of both validateAdmin and matchPropertyAdmin.
validateAdminAndMatchProperty :: MonadIO m =>
     ConfigES
  -> T.Text
  -> T.Text
  -> ReaderT MySQL.SqlBackend m (Either String (Integer, Int, Maybe T.Text, T.Text, [T.Text], T.Text, T.Text, T.Text))            
validateAdminAndMatchProperty coEs token propId = do 
  adminCreds <- validateAdmin token
  case adminCreds of
    Left error -> return $ Left error 
    Right (userId, canCreate) -> do
      match <- liftIO $ matchPropertyAdmin coEs userId propId
      return $ either (\err -> Left err)
                      (\(imgId, propType, facs, cCode, region, city)
                         -> Right (userId, canCreate, imgId, propType, facs, cCode, region, city))
                      match


-- Given a token and a bookableId verify if that bookable's propertyId matches the user identified by
-- that token.
validateAdminAndMatchBookable :: MonadIO m =>
     ConfigES
  -> T.Text
  -> T.Text
  -> ReaderT MySQL.SqlBackend m (Either String Integer)
validateAdminAndMatchBookable coEs token bookId = do
  eitherBookable <-  liftIO $ runExceptT $ queryAndParseBookable coEs bookId
  case eitherBookable of
    Left error -> return $ Left error
    Right bookable -> do   
      let propId = BT.propId $ BT.basicData bookable
      adminCreds <- validateAdminAndMatchProperty coEs token propId
      case adminCreds of
        Left error -> return $ Left error
        Right (userId, _, _, _, _, _, _, _) -> return $ Right userId   



-- Given a user token and a reservationId, verify if that reservation matches the user identified by that token.
-- The user matches either in case that the user is the one who made the reservation or in case
-- the user is the admin of the reservation's property.
matchUserAndReserv :: MonadIO m => ConfigMySQL
                                -> ConfigES
                                -> Integer
                                -> T.Text
                                -> ReaderT MySQL.SqlBackend m (Either String RT.Reservation)
matchUserAndReserv coMysql coEs reservId token = do
  maybeReserv <- RR.getReservById reservId
  case maybeReserv of
    Left error -> return $ reservNotFound
    Right repoReserv -> do
      let (RSC.Reservation _ userKey _ _ _ _ _ _ propId _ _ _ _ _ _ _ _) = repoReserv
          userId = fmap userKeyToInt userKey                          
          reservation = toDomainReserv (reservId, repoReserv)
      userCreds <- validateUser token
      adminCreds <- validateAdminAndMatchProperty coEs token (TE.decodeUtf8 propId)
      case (adminCreds, fmap (matchReservUser userId) userCreds) of
        (Right _, _) -> return $ Right reservation -- Admin validated.
        (_, Right True) -> return $ Right reservation -- Reservation User validated.
        (Left error1, Left error2) -> return $ Left error1
        (_, _) -> return $ userReservMismatch
  where
    matchReservUser id1 (id2, _, _, _, _) = id1 == Just id2
    toDomainReservState s = case s of
      DbT.Pending -> RT.Pending
      DbT.Accepted -> RT.Accepted
      DbT.Rejected -> RT.Rejected
      DbT.Absent -> RT.Absent


-- Set a default Int value
setBound :: Int -> Int -> Int
setBound defVal val = if val > 0 then val else defVal



-- Given a bookableId query it and parse it if possible.
queryAndParseBookable :: ConfigES
                        -> T.Text
                        -> ExceptT String IO BT.Bookable
queryAndParseBookable coEs bookId = do
  jsonRes <- liftIO $ runExceptT $ BR.queryBookableById coEs bookId
  case jsonRes of
    Left error -> ExceptT $ return bookNotFound 
    Right (Object object) -> do
      case parseBookable object of
        Nothing -> ExceptT $ return bookParsingErr
        Just bookable -> return bookable 


-- Given a propertyId query it and parse the bookables related to it it if possible.
queryAndParsePropBookables :: ConfigES
                           -> T.Text
                           -> ExceptT String IO [BT.Bookable]
queryAndParsePropBookables  coEs propId = do
  jsonRes <- liftIO $ runExceptT $ BR.queryBookablesByProp coEs propId
  case jsonRes of
    Left error -> ExceptT $ return bookNotFound 
    Right (Object object) -> do
      case parseBookables object of
        Nothing -> ExceptT $ return bookParsingErr
        Just bookable -> return bookable 



-- Given a propertyId get its mainImgId if possible.
queryAndParseProperty :: ConfigES
                      -> T.Text
                      -> IO (Maybe PT.Property)
queryAndParseProperty coEs propId = do
  jsonRes <- liftIO $ runExceptT $ PR.queryPropertyById  coEs propId
  case jsonRes of
    Left error -> return Nothing
    Right (Object object) -> return $ parseProperty object


queryAndParseEitherProperty :: ConfigES
                            -> T.Text
                            -> IO (Either String PT.Property)
queryAndParseEitherProperty coEs propId = do
  jsonRes <- liftIO $ runExceptT $ PR.queryPropertyById  coEs propId
  case jsonRes of
    Left error -> return propNotFound
    Right (Object object) -> do
      case parseProperty object of
        Nothing -> return propParsingErr 
        Just property -> return $ Right property


        
-- Given a bookableId, a start period (from), an end period (to) and a numDays, create a calendar which
-- starts from that start period, is as big as numDays, and has reservations included in an internval (from, to).
-- Note that it is only necessary to add the reservedPricings' list of rooms because when creating cancellations
-- those lists of rooms are updated by taking out the cancelled rooms; this makes calendar creation much more
-- efficient.
-- IMPORTANT: The reservations included in the calendar have a period of time of the form (checkIn, checkOut - 24h).
-- This is so because a (checkIn, checkOut) only takes into account the number of nights and the final day, the checkOut,
-- is handled particularly by each property admin.
createBookableCalendar :: MonadIO m => T.Text
                                    -> TM.UTCTime
                                    -> TM.UTCTime
                                    -> Int
                                    -> S.Set T.Text
                                    -> ReaderT MySQL.SqlBackend m (Maybe SCT.SCalendar)
createBookableCalendar bookId from to numDays roomIds = do
  query <- RR.getBklReservedPrisInPeriod_ bookId DbT.Accepted (from, to)
  let resPris = fmap toCalendarReserv query
  return $ do
    newCal <- SC.createCalendar from numDays 
    calendar <- SC.reserveManyPeriods_ resPris newCal
    return $ SCT.SCalendar roomIds calendar 
  where
    toCalendarReserv (resPriId, RSC.ReservedPricing _ _ _ cIn cOut _ _ _ _ _ rooms) = 
      let lastNight = TM.addUTCTime (-86400) cOut  -- create a calendar with intervals that span up to the last night.
      in SCT.Reservation (parseRooms rooms) (cIn, lastNight)



-- Verify if the given numebr of bytes is within (150kb, 5000kb)
checkImgBound :: Int -> Ordering
checkImgBound bytes
  | bytes < 150000 = LT -- 150kb
  | bytes > 5000000 = GT -- 5000kb
  | otherwise = EQ



-- A valid description must have at 31 characters but less than 2000.
validateDescription :: T.Text -> Either String T.Text
validateDescription desc
  | numChars < 31 = badDescription
  | numChars > 2001 = badDescription
  | otherwise = Right desc
  where
    numChars = T.length desc

-- Like validate description but with Maybe.
validateDescriptionM :: T.Text -> Maybe T.Text
validateDescriptionM desc = either (\e -> Nothing) (\v -> Just v) (validateDescription desc)


-- Validate an email string: A valid email string according to RFC 5322. Also an email string 
-- cannot be longer than characters.
validateEmail :: T.Text -> Either String T.Text
validateEmail email
  | EV.isValid byteString = Right email
  | T.length email > 51 = badEmail
  | otherwise = badEmail
  where
    byteString = TE.encodeUtf8 email



-- Validate a phone string: a valid phone srtring cannot be empty and must not be longer than 30 characters
validatePhone :: T.Text -> Either String T.Text
validatePhone phone
  | numChars < 3 = badPhone
  | numChars > 31 = badPhone
  | not (T.any isDigit phone) = badPhone
  | otherwise = Right phone
  where
    numChars = T.length phone 


-- A name-string cannot be empty and have less than 35
validateNameString :: T.Text -> Either String T.Text
validateNameString name
  | numChars < 3 = badNameString
  | numChars > 36 = badNameString
  | not (T.any isAlpha name) = badNameString
  | otherwise = Right name
  where
    numChars = T.length name


-- Custom Errors
noToken = Left "No Token" 
userNotFound = Left "User Not Found"  
malformedToken = Left "Bad Token"  
tooSmallImage = Left "Too Small Image"  
tooLargeImage = Left "Too Large Image"  
invalidSignature = Left "Invalid Signature"  
expiredToken = Left "Expired Token"  
passMismatch = Left "Password Mismatch" 
profNameOrEmailMismatch = Left "Profile Name Or Main Email Does Not Match"
blockedAdmin = Left "Blocked Admin" 
noSecret = Left "No Secret"
cannotCreate = Left "Cannot Create Properties" 
indexPropErr = Left "Error While Indexing Property" 
indexBookErr = Left "Error While Indexing Bookable"
badPropName = Left "Bad Property Name" 
badPropType = Left "Bad Property Type" 
badRegion = Left "Bad Region String" 
badAddress = Left "Bad Address String"  
badCity = Left "Bad City String"  
propNotFound = Left "Property Not Found" 
propParsingErr = Left "Error While Parsing Property ES Response" 
propAdminMismatch = Left "Property-Admin Mismatch" 
bookParsingErr = Left "Error While Parsing Bookable ES Response"
badDescription = Left "Bad Description" 
badFacilities = Left "Bad List Of Facilities" 
badRules = Left "Bad List Of Rules" 
badPhones = Left "Bad List Of Phones"
badReviewContent = Left "Bad Review Content"
badReviewScore = Left "Bad Review Score"
noAvProfName = Left "Unavailable Profile Name" 
badEmail = Left "Bad Email"  
badPhone = Left "Bad Phone" 
badPass = Left "Bad Password" 
badProfName = Left "Bad Profile Name" 
noAvMainEmail = Left "Unavailable Main Email" 
searchPropErr = Left "Error While Searching Indexed Properties" 
badImgId = Left "Bad Image Id" 
badNameString = Left "Bad Name String" 
tooMuchImgs = Left "Allowed Number of Images Exceeded"
badOccupancy = Left "Invalid Maximum Occupancy"    
badRoomIds = Left "Bad list of Room Ids"
badBookName = Left "Bad Bookable Name"
badBedNum = Left "Invalid Number Of Beds"
badBedType = Left "Bad Bed Type"
badRoomSize = Left "Bad Room Size"
badAmenities = Left "Bad List Of Amenities"
bookNotFound = Left "Bookable Not Found"
reviewNotFound = Left "Review Was Not Found"
updateBookErr = Left "Error While Updating Bookable"
updatePropErr = Left "Error While Updating Property"
emptyPris = Left "Empty List Of Pricings"
alreadyListed = Left "Bookable Already Listed"
bklNoDescs = Left "Bookable Has No Descriptions"
bklNoRooms = Left "Bookable Has No Room Ids"
bklNoImgs = Left "Bookable Has No Images"
maxOccuPriErr = Left "Max Occupancy Less Than Some Pricings Occupancy"
alreadyUnlisted = Left "Bookable Already Unlisted"
badPriOccupancy = Left "Invalid Pricing Occupancy" 
badPriConds = Left "Bad List Of Pricing Conditions" 
badPriPrice = Left "Bad Price Value" 
badPriDisc = Left "Bad Pricing Discount" 
priOccuTooLarge = Left "Pricing Occupancy Greater Than Max Occupancy"
priNotFound = Left "Pricing Not Found"
tooManyPricings = Left "Number Of Pricings Exceeded"
searchBookErr = Left "Error While Searching Indexed Bookables" 
bookPropMismatch = Left "Bookable And Property Don't Match"
tooManyRooms = Left "Number Of Rooms Greater Than Total Rooms"
greaterCinErr = Left "Check-In Greater Than Check-Out"
cInLessThanCurrent = Left "Check-In Less Than Current Day"
invalidNumRooms = Left "Invalid Number Of Rooms"
moreThan30Nights = Left "Cannot Reserve More Than 30 Nights"
lessThan1Night = Left "Cannot Reserve Less Than 1 Night"
roomsNotAvailable = Left "Some Rooms Not Available For Some Bookables"
emptyAssignedRooms = Left "Empty Set Of Assigned Rooms"
badAssignedRooms = Left "Assigned Rooms Not Subset Of Bookable's Total Rooms"
userReservMismatch = Left "User And Reservation Don't Match"
reservNotFound = Left "Reservation Not Found"
reservIdsMismatch = Left "Reservation And Reserved Pricing Don't Match"
assignedRoomsSizeMismatch = Left "Reserved Pricing and Assignment Number Of Rooms Don't Match"
notNullAssignedRooms = Left "ReservedPricing Has Already Been Assigned Rooms"
resPriNotFound = Left "Reserved Pricing Not Found"
cannotAcceptByAdmin = Left "Cannot Accept Reservation By Admin"
cannotAcceptNotPending = Left "Cannot Accept Reservation Not Pending"
cannotAcceptExpired = Left "Cannot Accept Expired Reservation"
unlistedBook = Left "Bookable Is Unlisted"
cannotRejectByAdmin = Left "Cannot Reject Reservation By Admin" 
cannotRejectNotPending = Left "Cannot Reject Reservation Not Pending" 
cannotRejectExpired = Left "Cannot Reject Expired Reservation" 
cannotAbsentNotAccepted = Left "Cannot Absent Reservation Not Accepted"
cannotAbsentArrived = Left "Cannot Absent Arrived Reservation"
cannotAbsentByAdmin = Left "Cannot Absent By Admin Reservation"
invalidAbsentDate = Left "Absent Only valid in (check-in + 24, check-out)"
notifNotFound = Left "Notification Not Found"
userNotifMismatch = Left "User And Notification Don't Match"
cannotArriveNotAccepted = Left "Cannot Mark As Arrived Not Accepted"
alreadyArrived = Left "Already Marked As Arrived"
cannotArriveBeforeCin = Left "Cannot Mark As Arrived Before Check-In"
cannotAbsentNotByAdmin = Left "Cannot Absent Not By Admin Reservation"
calendarCreationErr = Left "Calendar Could Not Be Created"
invalidCancelledRooms = Left "Cancelled Rooms Less Than 0"
tooManyCancelledRooms = Left "Cancelled Rooms More Than Reserved Rooms"
invalidCancellationState = Left "Cancellations Only Allowed For Accepted Or Pending Reservations"
cannotCancelExpired = Left "Cannot Cancel Expired Reservation"
cannotCancelAfterCheckIn = Left "Cannot Cancel After CheckIn"
cannotCancelByAdmin = Left "Cannot Cancel Reservation By Admin"
cannotCancelByUser = Left "Cannot Cancel Reservation By User"
assignmentsResPrisMismatch = Left "Assignments Don't Match Reserved Pricings"
reportCreationErr = Left "Report Could Not Be Created"
cannotReviewByAdmin = Left "Cannot Review Reservation By Admin"
cannotReviewNotAccepted = Left "Cannot Review Reservation Not Accepted"
cannotReviewBeforeCheckOut = Left "Cannot Review Reservation Before CheckOut"
alreadyReviewed = Left "Reservation Has Already Been Reviewed"
imgNotFound = Left "Image Not Found"   
decodeImgError = Left "Cannot Decode Image" 
reservMoreThanOneYearBefore = Left "Cannot Reserve With More Than One Year Of Anticipation"
cannotDeleteBkl = Left "Cannot Delete Bookable With Active Accepted Reservations"
bklDeletionErr = Left "Error While Deleting Bookable" 
emailErr = Left "Error While Authenticating Email Service"
noRecCode = Left "User Has Not Requested Recovery Code"
expiredRecCode = Left "Recovery Code Has Expired"
recCodeMismatch = Left "Recovery Code Does Not Match"

-- Custom Servant Errors
_imgNotFound = err404 { errBody = "Image Not Found" }
_userNotFound = err404 { errBody = "User Not Found" }
_propNotFound = err404 { errBody = "Property Not Found" }
_bookNotFound = err404 { errBody =  "Bookable Not Found" }
_priNotFound = err404 { errBody = "Pricing Not Found" }
_reviewNotFound = err404 {  errBody = "Review Was Not Found" }
_bklNoImgs = err404 { errBody = "Bookable Has No Images" }
_reservNotFound = err404 { errBody = "Reservation Not Found" }
_emptyPris = err400 { errBody = "Empty List Of Pricings" }
_tooManyPricings = err400 { errBody =  "Number Of Pricings Exceeded" }
_alreadyListed = err400 { errBody = "Bookable Already Listed" }
_bklNoDescs = err400 { errBody = "Bookable Has No Descriptions" }
_bklNoRooms = err400 { errBody = "Bookable Has No Room Ids" }
_priOccuTooLarge = err400 { errBody = "Pricing Occupancy Greater Than Max Occupancy" }
_alreadyUnlisted = err400 { errBody = "Bookable Already Unlisted" }
_malformedToken = err400 { errBody = "Malformed Token" }
_tooSmallImage = err400 { errBody = "Image Size Less Than 150KB" }
_tooLargeImage = err400 { errBody = "Image Size Greater Than 5MB" }
_badImgId = err400 { errBody = "Bad Image Id" }
_badPropName = err400 { errBody = "Bad Property Name" }
_badPropType = err400 { errBody = "Bad Property Type" }
_badDescription = err400 { errBody = "Bad Description Text" }
_badRegion = err400 { errBody = "Bad Region String" }
_cannotCancelByAdmin = err400 { errBody =  "Cannot Cancel Reservation By Admin" }
_badCity = err400 { errBody = "Bad City String" }
_badAddress = err400 { errBody = "Bad Address String" }
_badRoomIds = err400 { errBody = "Bad list of Room Ids" }
_badFacilities = err400 { errBody = "Bad List Of Facilities" }
_badRules = err400 { errBody = "Bad List Of Rules" }
_invalidNumRooms = err400 { errBody = "Invalid Number Of Rooms" }
_badPhones = err400 { errBody = "Bad List Of Phones" }
_badBedNum = err400 { errBody = "Invalid Number Of Beds" }
_badBedType = err400 { errBody = "Bad Bed Type" }
_badRoomSize = err400 { errBody = "Bad Room Size" }
_badReviewContent = err400 { errBody = "Bad Review Content" }
_badReviewScore = err400 { errBody = "Bad Review Score" }
_noAvProfName = err400 { errBody = "Profile Name Not Available" }
_noAvMainEmail = err400 { errBody = "Main Email Not Available" }
_tooMuchImgs = err400 { errBody = "Allowed Number of Images Exceeded" }
_badEmail = err400 { errBody = "Bad Email" }
_badPhone = err400 { errBody = "Bad Phone Number" }
_badPass = err400 { errBody = "Bad Password" }
_badAmenities = err400 {  errBody = "Bad List Of Amenities" }
_badNameString = err400 { errBody = "Bad Name String" }
_badProfName = err400 { errBody = "Bad Profile Name" }
_badOccupancy = err400 { errBody = "Invalid Maximum Occupancy" }
_badPriOccupancy = err400 { errBody = "Invalid Pricing Occupancy" }
_badPriConds = err400 { errBody = "Bad List Of Pricing Conditions" }
_badPriPrice = err400 { errBody = "Bad Price Value" }
_badPriDisc = err400 { errBody = "Bad Pricing Discount" }
_badBookName = err400 { errBody = "Bad Bookable Name" }
_bookPropMismatch = err400 { errBody =  "Bookable And Property Do Not Match" }
_tooManyRooms = err400 { errBody = "Number Of Rooms Greater Than Total Rooms" }
_unlistedBook = err400 { errBody = "Bookable Is Unlisted" }
_maxOccuPriErr = err400 { errBody = "Max Occupancy Less Than Some Pricings Occupancy" }
_greaterCinErr = err400 { errBody = "Check-In Greater Than Check-Out" }
_notifNotFound = err400 { errBody = "Notification Not Found" }
_assignmentsResPrisMismatch = err400 { errBody = "Assignments Do Not Match Reserved Pricings" }
_cannotAbsentByAdmin = err400 { errBody = "Cannot Absent By Admin Reservation" }
_emptyAssignedRooms = err400 { errBody = "Empty Set Of Assigned Rooms" }
_badAssignedRooms = err400 { errBody = "Assigned Rooms Not Subset Of Bookable's Total Rooms" }
_roomsNotAvailable = err400 {  errBody = "Some Rooms Not Available For Some Bookables" }
_cInLessThanCurrent = err400 { errBody = "CheckIn Less Than Current Day" }
_moreThan30Nights = err400 { errBody =  "Cannot Reserve More Than 30 Nights" }
_lessThan1Night = err400 { errBody = "Cannot Reserve Less Than 1 Night" }
_reservIdsMismatch = err400 { errBody = "Reservation And ReservedPricing Do Not Match" }
_cannotAbsentNotAccepted = err400 { errBody = "Cannot Absent Reservation Not Accepted" }
_reservMoreThanOneYearBefore = err400 { errBody = "Cannot Reserve With More Than One Year Of Anticipation" }
_invalidAbsentDate = err400 { errBody = "Absent Only Valid In (CheckIn + 24, CheckOut)" }
_cannotAbsentArrived = err400 { errBody = "Cannot Absent Arrived Reservation" }
_assignedRoomsSizeMismatch = err400 { errBody = "Reserved Pricing and Assignment Number Of Rooms Don't Match" }
_notNullAssignedRooms = err400 { errBody = "ReservedPricing Has Already Been Assigned Rooms" }
_resPriNotFound = err400 { errBody = "Reserved Pricing Not Found" }
_cannotAcceptByAdmin = err400 { errBody = "Cannot Accept Reservation By Admin" }
_cannotAcceptNotPending = err400 { errBody = "Cannot Accept Reservation Not Pending" }
_cannotAcceptExpired = err400 { errBody = "Cannot Accept Expired Reservation" }
_cannotRejectByAdmin = err400 { errBody = "Cannot Reject Reservation By Admin" }
_cannotRejectNotPending = err400 { errBody = "Cannot Reject Reservation Not Pending" }
_cannotRejectExpired = err400 { errBody = "Cannot Reject Expired Reservation" }
_cannotArriveNotAccepted = err400 {  errBody = "Cannot Mark As Arrived Not Accepted" }
_cannotCancelByUser = err400 { errBody = "Cannot Cancel Reservation By User" }
_alreadyArrived = err400 { errBody = "Already Marked As Arrived" }
_cannotArriveBeforeCin = err400 { errBody = "Cannot Mark As Arrived Before CheckIn" }
_cannotReviewByAdmin = err400 { errBody = "Cannot Review Reservation By Admin" }
_cannotReviewNotAccepted = err400 { errBody = "Cannot Review Reservation Not Accepted" }
_cannotReviewBeforeCheckOut = err400 { errBody = "Cannot Review Reservation Before CheckOut" }
_cannotAbsentNotByAdmin = err400 {  errBody = "Cannot Absent Not By Admin Reservation" }
_invalidCancelledRooms = err400 { errBody = "Cancelled Rooms Less Than 0" }
_tooManyCancelledRooms = err400 { errBody = "Cancelled Rooms More Than Reserved Rooms" }
_invalidCancellationState = err400 { errBody = "Cancellations Only Allowed For Accepted Or Pending Reservations" }
_cannotCancelExpired = err400 { errBody ="Cannot Cancel Expired Reservation" }
_cannotCancelAfterCheckIn = err400 { errBody = "Cannot Cancel After CheckIn" }
_alreadyReviewed = err400 { errBody = "Reservation Has Already Been Reviewed" }
_cannotDeleteBkl = err400 { errBody = "Cannot Delete Bookable With Active Accepted Reservations" }
_expiredRecCode = err400 { errBody = "Recovery Code Has Expired" }
_noRecCode = err400 { errBody = "User Has Not Requested Recovery Code" }
_recCodeMismatch = err401 { errBody = "Recovery Code Does Not Match" }
_invalidSignature = err401 { errBody = "Invalid Token Signature" }
_blockedAdmin = err401 { errBody = "Blocked Admin" }
_expiredToken = err401 { errBody = "Expired Token" }
_userNotifMismatch = err401 { errBody = "User And Notification Do Not Match" }
_propAdminMismatch = err401 { errBody = "Property And Admin Do Not Match" }
_profNameOrEmailMismatch = err401 { errBody = "Profile Name Or Main Email Does Not Match" }
_userReservMismatch = err401 { errBody = "User And Reservation Do Not Match" }
_passMismatch = err401 { errBody = "Pasword Mismatch" }
_noSecret = err401 { errBody = "No Shared Secret" }
_noToken = err403 { errBody = "No Token Provided" }
_cannotCreate = err403 { errBody = "User Cannot Create Properties" }
_indexPropErr = err500 { errBody = "Error While Indexing Property" }
_indexBookErr = err500 { errBody = "Error While Indexing Bookable" }
_propParsingErr = err500 { errBody = "Error While Parsing Property ES Response" }
_searchPropErr = err500 { errBody = "Error While Searching Indexed Properties" }
_searchBookErr = err500 { errBody = "Error While Searching Indexed Bookables" }
_bookParsingErr =  err500 { errBody = "Error While Parsing Bookable ES Response" } 
_updateBookErr = err500 { errBody = "Error While Updating Bookable" }
_updatePropErr = err500 { errBody = "Error While Updating Property" }
_decodeImgError = err500 { errBody = "Image Could Not Be Decoded" }
_calendarCreationErr = err500 { errBody =  "Calendar Could Not Be Created" }
_reportCreationErr = err500 { errBody = "Report Could Not Be Created" }
_bklDeletionErr = err500 { errBody = "Error While Deleting Bookable" } 
_emailErr = err500 { errBody = "Error While Authenticating Email Service" }
_unknown = err500 { errBody = "Unkown Error" }



-- From a Either String a get a Either ServantErr a          
getCustomError :: Eq a => Either String a -> Either ServantErr a   
getCustomError either
  | either == userNotFound = Left _userNotFound
  | either == noToken = Left _noToken
  | either == malformedToken = Left _malformedToken 
  | either == tooSmallImage = Left _tooSmallImage 
  | either == tooLargeImage = Left _tooLargeImage 
  | either == invalidSignature = Left _invalidSignature 
  | either == expiredToken = Left _expiredToken 
  | either == passMismatch = Left _passMismatch
  | either == blockedAdmin = Left _blockedAdmin
  | either == noSecret = Left _noSecret
  | either == cannotCreate = Left _cannotCreate
  | either == indexPropErr = Left _indexPropErr
  | either == badPropName = Left _badPropName
  | either == badPropType = Left _badPropType
  | either == badRegion = Left _badRegion
  | either == badAddress = Left _badAddress 
  | either == badCity = Left _badCity
  | either == propParsingErr = Left _propParsingErr
  | either == propNotFound = Left _propNotFound
  | either == propAdminMismatch = Left _propAdminMismatch
  | either == badDescription = Left _badDescription
  | either == badFacilities = Left _badFacilities
  | either == badRules = Left _badRules
  | either == badPhones = Left _badPhones
  | either == noAvProfName = Left _noAvProfName
  | either == badEmail = Left _badEmail 
  | either == badPhone = Left _badPhone
  | either == badPass = Left _badPass
  | either == badProfName =  Left _badProfName
  | either == noAvMainEmail = Left _noAvMainEmail
  | either == searchPropErr = Left _searchPropErr
  | either == badNameString = Left _badNameString
  | either == badImgId = Left _badImgId
  | either == tooMuchImgs = Left _tooMuchImgs
  | either == indexBookErr = Left _indexBookErr
  | either == bookParsingErr = Left _bookParsingErr
  | either == badRoomIds = Left _badRoomIds
  | either == badOccupancy = Left _badOccupancy
  | either == badBedNum = Left _badBedNum
  | either == badBookName = Left _badBookName
  | either == bookNotFound = Left _bookNotFound
  | either == updateBookErr = Left _updateBookErr
  | either == emptyPris = Left _emptyPris
  | either == alreadyListed = Left _alreadyListed
  | either == bklNoDescs = Left _bklNoDescs
  | either == bklNoRooms = Left _bklNoRooms
  | either == bklNoImgs = Left _bklNoImgs 
  | either == updatePropErr = Left _updatePropErr
  | either == alreadyUnlisted = Left _alreadyUnlisted
  | either == badAmenities = Left _badAmenities
  | either == maxOccuPriErr = Left _maxOccuPriErr
  | either == badPriOccupancy = Left _badPriOccupancy   
  | either == badPriConds = Left _badPriConds
  | either == badPriPrice = Left _badPriPrice
  | either == badPriDisc = Left _badPriDisc
  | either == priOccuTooLarge = Left _priOccuTooLarge
  | either == priNotFound = Left _priNotFound
  | either == tooManyPricings = Left _tooManyPricings
  | either == searchBookErr = Left _searchBookErr
  | either == bookPropMismatch = Left _bookPropMismatch 
  | either == tooManyRooms = Left _tooManyRooms 
  | either == unlistedBook = Left _unlistedBook
  | either == cInLessThanCurrent = Left _cInLessThanCurrent 
  | either == invalidSignature = Left _invalidSignature
  | either == invalidNumRooms = Left _invalidNumRooms
  | either == moreThan30Nights = Left _moreThan30Nights
  | either == lessThan1Night = Left _lessThan1Night
  | either == roomsNotAvailable = Left _roomsNotAvailable 
  | either == greaterCinErr = Left _greaterCinErr
  | either == emptyAssignedRooms = Left _emptyAssignedRooms 
  | either == badAssignedRooms = Left _badAssignedRooms 
  | either == userReservMismatch = Left _userReservMismatch
  | either == reservNotFound = Left _reservNotFound
  | either == reservIdsMismatch = Left _reservIdsMismatch 
  | either == assignedRoomsSizeMismatch = Left _assignedRoomsSizeMismatch 
  | either == notNullAssignedRooms = Left _notNullAssignedRooms
  | either == resPriNotFound = Left _resPriNotFound
  | either == cannotAcceptByAdmin = Left _cannotAcceptByAdmin
  | either == cannotAcceptNotPending = Left _cannotAcceptNotPending  
  | either == cannotRejectByAdmin = Left _cannotRejectByAdmin  
  | either == cannotRejectNotPending = Left _cannotRejectNotPending  
  | either == cannotRejectExpired = Left _cannotRejectExpired 
  | either == cannotAcceptExpired = Left _cannotAcceptExpired
  | either == cannotAbsentNotAccepted = Left _cannotAbsentNotAccepted
  | either == invalidAbsentDate = Left _invalidAbsentDate
  | either == cannotAbsentArrived = Left _cannotAbsentArrived
  | either == cannotAbsentByAdmin = Left _cannotAbsentByAdmin
  | either == cannotAbsentNotByAdmin = Left _cannotAbsentNotByAdmin
  | either == notifNotFound = Left _notifNotFound 
  | either == userNotifMismatch = Left _userNotifMismatch
  | either == alreadyArrived = Left _alreadyArrived 
  | either == cannotArriveBeforeCin = Left _cannotArriveBeforeCin  
  | either == cannotAbsentNotByAdmin = Left _cannotAbsentNotByAdmin
  | either == calendarCreationErr = Left _calendarCreationErr
  | either == reportCreationErr = Left _reportCreationErr
  | either == invalidCancelledRooms = Left _invalidCancelledRooms 
  | either == tooManyCancelledRooms = Left _tooManyCancelledRooms
  | either == invalidCancellationState = Left _invalidCancellationState 
  | either == cannotCancelExpired = Left _cannotCancelExpired 
  | either == cannotCancelAfterCheckIn = Left _cannotCancelAfterCheckIn 
  | either == cannotCancelByAdmin = Left _cannotCancelByAdmin
  | either == cannotCancelByUser = Left _cannotCancelByUser
  | either == assignmentsResPrisMismatch = Left _assignmentsResPrisMismatch 
  | either == badReviewContent =  Left _badReviewContent 
  | either == badReviewScore = Left _badReviewScore 
  | either == cannotReviewByAdmin = Left _cannotReviewByAdmin 
  | either == cannotReviewNotAccepted = Left _cannotReviewNotAccepted 
  | either == cannotReviewBeforeCheckOut = Left _cannotReviewBeforeCheckOut 
  | either == alreadyReviewed = Left _alreadyReviewed
  | either == reviewNotFound = Left _reviewNotFound
  | either == profNameOrEmailMismatch = Left _profNameOrEmailMismatch
  | either == cannotDeleteBkl = Left _cannotDeleteBkl
  | either == reservMoreThanOneYearBefore = Left _reservMoreThanOneYearBefore
  | either == bklDeletionErr = Left _bklDeletionErr
  | either == emailErr = Left _emailErr
  | either == expiredRecCode = Left _expiredRecCode
  | either == noRecCode = Left _noRecCode 
  | either == recCodeMismatch = Left _recCodeMismatch
getCustomError either = case either of
  Left _ -> Left _unknown 
  Right a -> Right a



getImgCustomError either = case either of
  Left "Image Not Found" -> Left _imgNotFound 
  Left "Cannot Decode Image" -> Left _decodeImgError
  Left _ -> Left _unknown
  Right v -> Right v
