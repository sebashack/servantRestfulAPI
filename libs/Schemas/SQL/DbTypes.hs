{-# LANGUAGE TemplateHaskell #-}

module Schemas.SQL.DbTypes where

import Database.Persist.TH

data Gender = Male | Female | NoGen deriving (Show, Read, Eq)
derivePersistField "Gender" 

data ReservState = Pending | Accepted | Rejected | Absent deriving (Show, Read, Eq)
derivePersistField "ReservState" 
