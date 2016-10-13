{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Schemas.SQL.User where

import Configs.MySQL 
import HelperLibs.MySQL.ActionRunner
import Schemas.SQL.DbTypes
import Data.ByteString (ByteString)
import Data.Time
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Database.Persist as Per

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "userModel"] [TH.persistLowerCase|
User
  created           UTCTime 
  profileName       ByteString
  name              T.Text
  lastName          T.Text
  birthDate         UTCTime
  country           T.Text
  gender            Gender
  mainEmail         ByteString
  secondEmail       T.Text Maybe
  phoneNum          T.Text Maybe
  blockedAdmin      Bool  -- An state which tells us if this is Blocked Admin.
  canCreate         Int     
  password          ByteString  -- Hashed password.
  secret            ByteString Maybe
  secretExp         UTCTime Maybe
  recCode           ByteString Maybe  -- Hashed recovery code.
  recCodeExp        UTCTime Maybe
  UniqueUserEmail mainEmail
  UniqueProfileName profileName
  deriving Show
ProfileAvatar
  avatarId          ByteString
  userId            UserId
  image             ByteString -- mediumblob
  UniqueAvatar userId
  UniqueUserAvatar avatarId
  deriving Show
|]



printDevelUser :: IO ()
printDevelUser = runMySQL mysqlDevelConfig $ MySQL.printMigration userModel
