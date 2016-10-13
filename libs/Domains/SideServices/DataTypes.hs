{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.SideServices.DataTypes where

import qualified Data.Text as T
import GHC.Generics  
import Data.Aeson
import Data.Aeson.Types


data ResetPassData = ResetPassData {
  profName :: T.Text,
  recoveryCode :: T.Text,
  newPassword :: T.Text
  } deriving (Show, Eq, Generic)            

instance FromJSON ResetPassData
instance ToJSON ResetPassData

