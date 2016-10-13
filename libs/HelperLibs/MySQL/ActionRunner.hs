{-# LANGUAGE FlexibleContexts      #-}

module HelperLibs.MySQL.ActionRunner where

import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.Sql as SQL
import qualified Database.MySQL.Base.Types as MySQLTypes
import Configs.ConfigTypes
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control

-- This function receives a ConfigMySQL and perfomrs an action.
runMySQL :: (MonadIO m, MonadBaseControl IO m) =>
            ConfigMySQL
         -> SQL.SqlPersistM a
         -> m a
runMySQL config action =
  runStdoutLoggingT $ MySQL.withMySQLPool (connectionInfo config)
                                          (poolConnections config)
                                          (\pool -> liftIO $ SQL.runSqlPersistMPool action pool)
                                          
                                          
                                                                                        
