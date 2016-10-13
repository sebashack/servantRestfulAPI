module Configs.ConfigTypes where

import qualified Data.Text as T
import System.Environment
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import qualified Database.Persist.MySQL as MySQL

data Environment
  = Development | Production | Test
  deriving (Eq, Show)

-- This data type is intended to represent the configuration settings
-- to create an index in Elastic Search.
data ConfigES = ConfigES {
  host :: String,
  port :: Int,
  indexName :: String,
  numShards :: Int,
  numReplicas :: Int,
  analyzer :: T.Text,
  filter :: Maybe T.Text 
  } deriving (Eq, Show)


-- This data type is intended to represent the configurations settings
-- to create a MySQL conecction.
data ConfigMySQL = ConfigMySQL {
  connectionInfo :: MySQL.ConnectInfo,
  poolConnections ::Int
  } deriving (Eq, Show)

-- This data type is intended to embrace all configurations needed to run
-- the API server.
data ConfigAPI = ConfigAPI {
  configBookingES :: ConfigES,
  configContentAdminEs :: ConfigES,
  configMySQL :: ConfigMySQL
  } deriving (Eq, Show)


-- Given an Environment, this function sets a appropriate logger.
setApiLogger :: Environment -> Middleware
setApiLogger Test = id
setApiLogger Development = logStdoutDev
setApiLogger Production = logStdout


-- This function get the current environment of the API.
getAppEnv :: IO Environment
getAppEnv = do
  env <- lookupEnv "API_ENV"
  case env of
    Just "production" -> return Production
    Just "Test" -> return Test
    _ -> return Development


-- This function gets the current port of the API.
getAppPort :: IO Int
getAppPort = do
  port <- lookupEnv "API_PORT"
  case port of
    Nothing -> return 8081
    Just p -> return $ read p
