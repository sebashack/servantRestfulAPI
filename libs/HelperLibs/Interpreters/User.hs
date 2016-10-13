{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.Interpreters.User where

import Data.Aeson
import Data.Char
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Maybe
import HelperLibs.Interpreters.BookingDomain
import qualified Data.Text as T
import qualified Schemas.SQL.DbTypes as DbT
import qualified Domains.UserDomain.User.DataTypes as UT
import qualified Data.Map.Strict as Map
import qualified Web.JWT as JWT
import qualified Text.Email.Validate as EV
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
import qualified Database.Persist.MySQL as MySQL
import qualified Repositories.UserRepo.Operations as UR
import qualified Data.ByteString as SB (ByteString)



-- Transform a Domain Gender to a Repo Gender.
toRepoGender :: UT.Gender -> DbT.Gender
toRepoGender gen
  | gen == UT.Male = DbT.Male
  | gen == UT.Female = DbT.Female
  | otherwise = DbT.NoGen


-- Transform a Repo Gender to a Domain Gender.
toDomainGender :: DbT.Gender -> UT.Gender 
toDomainGender gen
  | gen == DbT.Male = UT.Male
  | gen == DbT.Female = UT.Female
  | otherwise = UT.NoGen



-- A profile name must have at least 4 characters but less than 25 
-- All characters in a profile name must be alphanumeric
validateProfName :: T.Text -> Either String T.Text
validateProfName name 
  | numChars < 4 = badProfName
  | numChars > 26 = badProfName
  | not (T.all isAlphaNum name) = badProfName
  | otherwise = Right name
  where
    numChars = T.length name 


  

{-
Given a BasicUserData data type this function validates it and returns another BasicUserData with all fields validated.
Note that this function also accepts optional oldProfileName and oldMainEmail parameters so that availability of 
these strings can be propery verified.
In case that second email or phone number is not valid, a Nothing is placed in the returned entity.
Also note that the birth date in the BasicUserData returned only takes into account the "year-month-day" and no the exact
momento. Thus something like (TM.UTCTime birth 0) is returned.
-}
validateBasicUserData :: MonadIO m => UT.BasicUserData
                                   -> Maybe T.Text
                                   -> Maybe T.Text
                                   -> ReaderT MySQL.SqlBackend m (Either String UT.BasicUserData)
validateBasicUserData userData oldProfName oldMainEmail = 
  case validatedData of
    Left error -> return $ Left error
    Right (optEmail, optPhone) -> do
      pred1 <- UR.profileNameAvailability profName
      pred2 <- UR.emailAvailability mEmail
      case (predicate pred1 profName oldProfName, predicate pred2 mEmail oldMainEmail) of
        (False, _) -> return noAvProfName
        (_, False) -> return noAvMainEmail
        (True, True) -> return $ Right (UT.BasicUserData profName fName lName (TM.UTCTime birth 0) cCode gen mEmail optEmail optPhone)
  where
    (UT.BasicUserData profName fName lName bDate cCode gen mEmail sEmail phNum) = userData
    (TM.UTCTime birth _) = bDate 
    validateOptionalField :: Maybe T.Text -> (T.Text -> Either String T.Text) -> Maybe T.Text
    validateOptionalField field func =
      case fmap func field of
        Nothing -> Nothing
        Just (Left error) -> Nothing
        Just (Right v) -> Just v
    validatedData = do 
      -- Optional Fields
      let sEmail' = validateOptionalField sEmail validateEmail
          phNum' = validateOptionalField phNum validatePhone
      -- Obligatory Fields
      validateProfName profName
      validateEmail mEmail
      validateNameString fName
      validateNameString lName
      return (sEmail', phNum')
    predicate bool newStr maybeStr = case maybeStr of
      Nothing -> bool
      Just str -> str == newStr || bool  
      


-- Like validate basic user data but also validated the password within CreateUserData.
validateCreateUserData :: MonadIO m => UT.CreateUserData 
                                    -> ReaderT MySQL.SqlBackend m (Either String UT.CreateUserData)
validateCreateUserData userData = do
  eitherBasicData <- validateBasicUserData (UT.basicData userData) Nothing Nothing
  case eitherBasicData of
    Left error -> return $ Left error
    Right validatedData ->
      case validatePassword $ UT.password userData of
        Left error -> return $ Left error
        Right validatedPass -> return $ Right $ UT.CreateUserData validatedData validatedPass  



-- Given a userId, a profileName and a secret, create an authentication token.
createUserToken :: Integer
                -> T.Text
                -> T.Text 
                -> IO (T.Text, TM.UTCTime)
createUserToken userId profName secret = do
  -- to transform Integer to UTC user
  -- TM.addUTCTime <numberOfSeconds> unixOrigin
  currentDate <- TM.getCurrentTime
  let tokenId = T.pack $ (show currentDate) ++ "_" ++ (show userId)   
      issuedAt = TM.diffUTCTime currentDate unixOrigin
      expiration = issuedAt + 691200 -- token expires 8 days after creation 
      payload = JWT.JWTClaimsSet (JWT.stringOrURI "destinosIO API")
                                 (JWT.stringOrURI "signed userId")
                                 audience
                                 (JWT.numericDate expiration)
                                 Nothing
                                 (JWT.numericDate issuedAt)
                                 (JWT.stringOrURI tokenId)
                                 (Map.fromList [("userId", toJSON userId), ("profName", toJSON profName)])
  return $ (JWT.encodeSigned JWT.HS256 (JWT.secret secret) payload,
            TM.addUTCTime expiration unixOrigin)
  where
    unixOrigin = TM.UTCTime (TM.fromGregorian 1970 01 01) 0
    audience = do
      aud <- JWT.stringOrURI "destinosIO API users"
      return $ Right [aud]
       

    

   

