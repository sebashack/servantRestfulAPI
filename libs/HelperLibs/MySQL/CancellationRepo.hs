{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.MySQL.CancellationRepo where

import qualified Schemas.SQL.Reservation as SCH
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Database.Persist as Per




cancEntityToTuple (Per.Entity key entity) =
  let eId = toInteger $ MySQL.unSqlBackendKey (SCH.unCancellationKey key)
  in (eId, entity)
