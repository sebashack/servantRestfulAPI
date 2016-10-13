{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Schemas.SQL.BookableImage where

import Configs.MySQL 
import HelperLibs.MySQL.ActionRunner
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Database.Persist as Per

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "bookableImageModel"] [TH.persistLowerCase| 
BookableImage
  imageId                ByteString
  bookableId             ByteString
  image                  ByteString -- mediumblob
  UniqueBookableImage imageId
  deriving Show
|]


printDevelBklImg :: IO ()
printDevelBklImg = runMySQL mysqlDevelConfig $ MySQL.printMigration bookableImageModel
