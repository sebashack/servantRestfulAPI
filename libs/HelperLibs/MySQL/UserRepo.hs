{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.MySQL.UserRepo where

import Schemas.SQL.DbTypes
import Data.Time 
import Control.Monad (replicateM)
import System.Random
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Schemas.SQL.User as SCH
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as TM
import qualified Database.Persist as Per

            

-- This function generates a unique random id code for an avatar.

genAvatarId :: IO ByteString
genAvatarId = do
  (TM.UTCTime date time) <- TM.getCurrentTime
  let (year, monthDay) = splitAt 4 (show date)
      (month, day') = splitAt 2 (tail monthDay)
      day = tail day'
      hour = show $ round (time * 1000000)
  part1 <- replicateM 2 (randomRIO ('a', 'z'))
  part2 <- replicateM 2 (randomRIO ('0', '9'))
  part3 <- replicateM 2 (randomRIO ('A', 'Z'))
  part4 <- replicateM 2 (randomRIO ('0', '9'))
  part5 <- replicateM 2 (randomRIO ('a', 'z'))
  part6 <- replicateM 2 (randomRIO ('A', 'Z'))
  return $ TE.encodeUtf8 $ T.pack (part1 ++
                                   part2 ++
                                   part3 ++
                                   part4 ++
                                   part5 ++
                                   part6 ++
                                   (drop 1 year) ++
                                   day ++
                                   month ++
                                   hour)



-- Given a userId this function generates a unique random secret needed to decrypt a user token.

genSecret :: Integer -> IO ByteString
genSecret userId = do
  (TM.UTCTime date time) <- TM.getCurrentTime
  let (year, monthDay) = splitAt 4 (show date)
      (month, day') = splitAt 2 (tail monthDay)
      day = tail day'
      hour = show $ round (time * 1000000)
  part1 <- replicateM 3 (randomRIO ('a','z'))
  part2 <- replicateM 3 (randomRIO ('0','9'))
  part3 <- replicateM 3 (randomRIO ('A','Z'))
  part4 <- replicateM 3 (randomRIO ('a','z'))
  return $ TE.encodeUtf8 $ T.pack (part1++part2++part3++(show userId)++part4++(drop 1 year)++month++day++hour)
  
  
  
  
