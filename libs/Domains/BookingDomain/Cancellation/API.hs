{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domains.BookingDomain.Cancellation.API 
       (
        CancellationAPI
       ) where


import Domains.BookingDomain.Cancellation.DataTypes
import Servant.API
import qualified Data.Time as TM
import qualified Data.Text as T

type UserToken = T.Text
type ReservId = Integer
type PropertyId = T.Text


type CancellationAPI =
       -- 1 --
       "reservation" :> "cancellations"
                     :> Capture "reservationId" ReservId
                     :> Header "Authorization" UserToken                     
                     :> Get '[JSON] [Cancellation]                        
       -- 2 --
  :<|> "user" :> "cancellation"
              :> Capture "reservationId" ReservId
              :> ReqBody '[JSON] CancellationData        
              :> Header "Authorization" UserToken
              :> PostCreated '[JSON] Cancellation
       -- 3 --
  :<|> "admin" :> "cancellation"
               :> Capture "reservationId" ReservId
               :> ReqBody '[JSON] CancellationData          
               :> Header "Authorization" UserToken
               :> PostCreated '[JSON] Cancellation          
       -- 4 --
  :<|> "property" :> "cancellations"
                   :> Capture "propertyId" PropertyId
                   :> QueryParam "from" Int
                   :> QueryParam "size" Int
                   :> Header "Authorization" UserToken                     
                   :> Get '[JSON] [Cancellation]
      


