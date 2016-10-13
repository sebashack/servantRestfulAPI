{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.UserDomain.User.DataTypes where

import qualified Data.Time as TM
import qualified Data.Text as T
import GHC.Generics  
import Data.Aeson
import Data.Aeson.Types
import Data.CountryCodes


data Gender = Male | Female | NoGen
  deriving (Eq, Show, Generic)

instance FromJSON Gender
instance ToJSON Gender


data BasicUserData = BasicUserData {
  profileName :: T.Text,
  name :: T.Text,
  lastName :: T.Text,
  birthDate :: TM.UTCTime,
  countryCode :: CountryCode,
  gender :: Gender,
  mainEmail :: T.Text,
  secondEmail :: Maybe T.Text,
  phoneNum :: Maybe T.Text
  } deriving (Show, Generic, Eq)

instance FromJSON BasicUserData
instance ToJSON BasicUserData


data CreateUserData = CreateUserData {
  basicData :: BasicUserData,
  password :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreateUserData
instance ToJSON CreateUserData


data User = User {
  userData :: BasicUserData,
  creationDate :: TM.UTCTime,
  blockedAdmin :: Bool, -- If true, bookables will be unlisted and admin endpoints blocked.
  canCreate :: Int, -- This is the number of properties a user is allowed to create.
  avatarId :: Maybe T.Text
  } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User


-- This is a convenience type to immediately login a user when he creates his account.
data NewUser = NewUser {
  user :: User,
  authToken :: T.Text
  } deriving (Show, Generic, Eq)

instance FromJSON NewUser
instance ToJSON NewUser



-- Two users are equal either if their profile names are equal, or if their main emails are equal.
-- Thus, profile names and main emails are unique identifiers.
instance Eq User where
  (User bd1 _ _ _ _) == (User bd2 _ _ _ _)
    = profileName bd1 == profileName bd2 || mainEmail bd1 == mainEmail bd2

      
data ChangePassData = ChangePassData {
  oldPassword :: T.Text,
  newPassword :: T.Text
  } deriving (Show, Eq, Generic)            

instance FromJSON ChangePassData
instance ToJSON ChangePassData


data TokenInfo = TokenInfo {
  token :: T.Text,
  expiration :: TM.UTCTime
  } deriving (Show, Eq, Generic)            


instance FromJSON TokenInfo
instance ToJSON TokenInfo





