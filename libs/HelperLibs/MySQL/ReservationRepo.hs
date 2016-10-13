{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.MySQL.ReservationRepo where

import Data.Time 
import Control.Monad (replicateM)
import System.Random
import Data.List
import Numeric (showHex)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Schemas.SQL.Reservation as SCH
import qualified Schemas.SQL.User as USCH
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as TM
import qualified Database.Persist as Per



-- Given the reservationId, generate a unique reservation code.
genReservCode :: Integer -> IO ByteString
genReservCode reservId = do
  let hexId = (showHex $ reservId * 10) ""
  part1 <- replicateM 2 (randomRIO ('a', 'z'))
  part2 <- replicateM 2 (randomRIO ('0', '9'))
  part3 <- replicateM 2 (randomRIO ('a', 'z'))
  return $ TE.encodeUtf8 $ T.pack (part1 ++ part2 ++ part3 ++  hexId)
 


-- Transform a Reservation entity into a Tuple
reservEntityToTuple (Per.Entity key entity) =
  let eId = toInteger $ MySQL.unSqlBackendKey (SCH.unReservationKey key)
  in (eId, entity)



-- Transform a ReservedPricing entity into a Tuple
reservedPrisEntityToTuple (Per.Entity key entity) =
  let eId = toInteger $ MySQL.unSqlBackendKey (SCH.unReservedPricingKey key)
  in (eId, entity)







