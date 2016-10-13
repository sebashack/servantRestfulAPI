{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.MySQL.ReviewRepo where


import qualified Schemas.SQL.Reservation as SCH
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Database.Persist as Per



reviewEntityToTuple (Per.Entity key entity) =
  let eId = toInteger $ MySQL.unSqlBackendKey (SCH.unReviewKey key)
  in (eId, entity)
