{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Schemas.SQL.PropertyImage where

import Configs.MySQL 
import HelperLibs.MySQL.ActionRunner
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Database.Persist as Per

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "propertyImageModel"] [TH.persistLowerCase| 
PropertyImage
  imageId                ByteString
  propertyId             ByteString
  image                  ByteString -- mediumblob
  UniquePropertyImage imageId
  deriving Show
|]


printDevelPropImg :: IO ()
printDevelPropImg = runMySQL mysqlDevelConfig $ MySQL.printMigration propertyImageModel



