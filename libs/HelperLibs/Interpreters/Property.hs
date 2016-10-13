{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.Interpreters.Property where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Servant
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import HelperLibs.ElasticSearch.ResponseParser
import Control.Monad.Except
import Configs.ConfigTypes
import HelperLibs.MySQL.ActionRunner
import HelperLibs.Interpreters.BookingDomain
import qualified Data.Text as T
import qualified Domains.BookingDomain.Property.DataTypes as PT
import qualified Domains.BookingDomain.Bookable.DataTypes as BT
import qualified Repositories.PropertyRepo.Operations as PR
import qualified Data.CountryCodes as CC
import qualified Schemas.SQL.DbTypes as DbT
import qualified Data.Map.Strict as Map
import qualified Web.JWT as JWT
import qualified Text.Email.Validate as EV
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE
import qualified Database.Persist.MySQL as MySQL
import qualified Data.ByteString as SB (ByteString)



-- This is an abstraction to update a property field: privde a propertyId, a value to update, a validation function to
-- validate that value and a function which updates that value.
--  either (\err -> throwError err) (\val -> return ()) (getCustomError update)          
updateProperty ::  ConfigMySQL
                -> ConfigES
                -> T.Text
                -> T.Text
                -> t
                -> (t -> Either String t1)
                -> (ConfigES -> T.Text -> t1 -> ExceptT e IO a)
                -> IO (Either String ())                
updateProperty coMysql coEs propId token value validationFunc updateFunc = do
  liftIO $ runMySQL coMysql $ do
    adminCreds <- validateAdminAndMatchProperty coEs token propId
    case adminCreds of
      Left error -> return $ Left error
      Right _ ->
        case validationFunc value of
          Left error -> return $ Left error
          Right value' -> do
            jsonRes <- liftIO $ runExceptT $ updateFunc coEs propId value'
            return $ either (\err -> updatePropErr) (\val -> Right ()) jsonRes


  
updatePropertyBooks :: ConfigES
                    -> T.Text
                    -> a
                    -> (ConfigES -> a -> BT.Bookable -> ExceptT String IO Value)
                    -> ExceptT ServantErr IO ()
updatePropertyBooks coEs propId val updateBook = do
  eitherBookables <- liftIO $ runExceptT $ queryAndParsePropBookables coEs propId
  case getCustomError eitherBookables of
    Left error -> throwError error
    Right bookables -> do
      bookUpdate <- liftIO $ runExceptT $ mapM (updateBook coEs val) bookables
      case bookUpdate of
        Left _ -> throwError _updateBookErr
        Right _ -> return ()



-- Validate an imgId: a valid image Id cannot be empty and cannot have more than 40 characters.
validateImgId :: Maybe T.Text ->  Either String (Maybe T.Text)
validateImgId Nothing = Right Nothing
validateImgId (Just imgId)
  | numChars < 1 = badImgId
  | numChars > 41 = badImgId
  | otherwise = Right $ Just imgId
  where 
    numChars = T.length imgId
    


-- Property names must be greater than 4 characters and less than 31.
validatePropName :: T.Text -> Either String T.Text
validatePropName name
  | numChars < 4 = badPropName
  | numChars > 31 = badPropName
  | not (T.any isAlphaNum name) = badPropName
  | otherwise = Right name
  where
    numChars = T.length name 
    


-- Property types must be greater than 4 characters and less than 21.
validatePropType :: T.Text -> Either String T.Text
validatePropType propType
  | numChars < 4 = badPropType
  | numChars > 21 = badPropType
  | not (T.any isAlphaNum propType) = badPropType
  | otherwise = Right propType
  where
    numChars = T.length propType 
    


-- Region names cannot be empty and cannot be greater than 35 characters.
validateRegion :: T.Text -> Either String T.Text
validateRegion region
  | numChars < 3 = badRegion
  | numChars > 35 = badRegion
  | otherwise = Right region
  where
    numChars = T.length region 
    
    

-- City names cannot be empty and cannot have more than 35 characters.
validateCity :: T.Text -> Either String T.Text
validateCity city
  | numChars < 3 = badCity
  | numChars > 35 = badCity
  | otherwise = Right city
  where
    numChars = T.length city
    
    

-- Addressess cannot be empty and cannot have more than 35 characters.
validateAddress :: T.Text -> Either String T.Text
validateAddress address
  | numChars < 1 = badAddress
  | numChars > 35 = badAddress
  | otherwise = Right address
  where
    numChars = T.length address 
    


-- A zip code cannot be empty and greater than 6 characters.
validateZipCode :: Maybe T.Text -> Maybe T.Text
validateZipCode Nothing = Nothing
validateZipCode (Just zipCode)
  | not (T.all isAlphaNum zipCode) = Nothing
  | numChars < 3 = Nothing
  | numChars > 6 = Nothing
  | otherwise = Just zipCode
  where
    numChars = T.length zipCode
    

  

-- Validate the information needed to update a Location. This function combines validations for
-- region, city and address.
validateRegionData :: PT.Region ->  Either String (T.Text, T.Text, T.Text, Maybe T.Text)
validateRegionData regionData = do
  validateRegion region
  validateCity city
  validateAddress address
  return (region, city, address, zipCode')
  where
    PT.Region region city address zipCode = regionData
    zipCode' = (validateZipCode zipCode)




-- Validate the basic information to create a property.This function combines validations for
-- location, property name, and property type.
validateBasicPropData :: PT.BasicPropData -> Either String PT.BasicPropData
validateBasicPropData propData = do
  validatePropName propName
  validatePropType propType
  validateRegion region
  validateCity city
  validateAddress address
  return $ PT.BasicPropData propName propType location'
  where
    PT.BasicPropData propName propType location = propData
    PT.Location cCode region city address zipCode = location
    location' = PT.Location cCode region city address (validateZipCode zipCode)




-- Validate a list of facilities: A valid list of facilities must not be greater than 200 elements, and each element must be
-- less than 50 characters long. Empty Strings are not allowed.
validateFacilities :: [T.Text] -> Either String [T.Text]
validateFacilities facilities 
  | length facilities > 200 = badFacilities
  | all isValidFac facilities = Right facilities
  | otherwise = badFacilities
  where
    isValidFac fac
      | numChars < 3 = False
      | numChars > 50 = False
      | not (T.any isAlpha fac) = False
      | otherwise = True
      where
        numChars = T.length fac




-- Validate a list of rules: A valid list of rules must not be greater than 50 elements, and each element must be
-- less than 301 characters long. Empty Strings are not allowed. 
validateRules :: [T.Text] -> Either String [T.Text]
validateRules rules
  | length rules > 50 = badRules
  | all isValidRule rules = Right rules
  | otherwise = badRules
  where
    isValidRule rl
      | numChars < 3 = False
      | numChars > 300 = False
      | not (T.any isAlpha rl) = False
      | otherwise = True
      where
        numChars = T.length rl 



-- Validate a list of phones: A valid list of phones must not be greater than 10 elements, and each element must be
-- less than 30 characters long. Empty Strings are not allowed. 
validatePhones :: [T.Text] -> Either String [T.Text]
validatePhones phones
  | length phones > 10 = badPhones
  | all isValidPhone phones = Right phones
  | otherwise = badPhones
  where
    isValidPhone ph
      | T.length ph < 3 = False
      | not (T.any isDigit ph) = False
      | T.length ph > 30 = False
      | otherwise = True





      
