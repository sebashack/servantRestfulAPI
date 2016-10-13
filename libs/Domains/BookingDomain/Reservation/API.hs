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

module Domains.BookingDomain.Reservation.API 
       (
        ReservationAPI
       ) where


import Domains.BookingDomain.Reservation.DataTypes
import Servant.API
import qualified Data.Time as TM
import qualified Data.Text as T

type ReservId = Integer
type ReservCode = T.Text
type UserToken = T.Text
type BookableId = T.Text
type PropertyId = T.Text
type NotifId = Integer
type Message = T.Text


type ReservationAPI =
       -- 1 --
       "reservation" :> Capture "reservationId" ReservId
                     :> Header "Authorization" UserToken                     
                     :> Get '[JSON] Reservation                
       -- 2 --              
  :<|> "reservation" :> "pricingInfo"
                     :> Capture "reservationId" ReservId
                     :> Header "Authorization" UserToken
                     :> Get '[JSON] [PricingInfo]
       -- 3 --
  :<|> "user" :> "reservation"
              :> Header "Authorization" UserToken
              :> ReqBody '[JSON] UserReservData
              :> PostCreated '[JSON] Reservation
       -- 4 --
  :<|> "admin" :> "reservation"
               :> Header "Authorization" UserToken
               :> ReqBody '[JSON] AdminReservData
               :> PostCreated '[JSON] Reservation
       -- 5 --
  :<|> "property" :> "reservations"
                  :> Capture "propertyId" PropertyId
                  :> QueryParam "state" T.Text  
                  :> QueryParam "from" Int
                  :> QueryParam "size" Int
                  :> Header "Authorization" UserToken                     
                  :> Get '[JSON] [Reservation]
       -- 6 --
  :<|> "userReservs" :> QueryParam "state" T.Text 
                     :> QueryParam "from" Int
                     :> QueryParam "size" Int
                     :> Header "Authorization" UserToken                     
                     :> Get '[JSON] [Reservation]
       -- 7 --
  :<|> "bookable" :> "reservationInfo"
                  :> Capture "bookableId" BookableId
                  :> QueryParam "state" T.Text
                  :> QueryParam "from" Int
                  :> QueryParam "size" Int
                  :> Header "Authorization" UserToken                     
                  :> Get '[JSON] [PricingInfo]
       -- 8 --            
  :<|> "reservation" :> "accept"
                     :> Capture "reservationId" ReservId
                     :> ReqBody '[JSON] Acceptance
                     :> Header "Authorization" UserToken
                     :> PostCreated '[JSON] Notification                    
       -- 9 --            
  :<|> "reservation" :> "reject"
                     :> Capture "reservationId" ReservId
                     :> ReqBody '[JSON] Message
                     :> Header "Authorization" UserToken
                     :> PostCreated '[JSON] Notification              
       -- 10 --            
  :<|> "reservation" :> "user"
                     :> "absent"
                     :> Capture "reservationId" ReservId
                     :> ReqBody '[JSON] Message
                     :> Header "Authorization" UserToken
                     :> PostCreated '[JSON] Notification           
       -- 11 --            
  :<|> "reservation" :> "admin"
                     :> "absent"
                     :> Capture "reservationId" ReservId
                     :> Header "Authorization" UserToken
                     :> Put '[JSON] ()                                 
       -- 12 --
  :<|> "userNotifs" :> QueryParam "from" Int
                    :> QueryParam "size" Int
                    :> Header "Authorization" UserToken                     
                    :> Get '[JSON] [Notification]                  
       -- 13 --
  :<|> "userNotif" :> Capture "notificationId" NotifId
                   :> Header "Authorization" UserToken                     
                   :> Get '[JSON] Notification
       -- 14 --
  :<|> "reservation" :> ReqBody '[JSON] ReservCode 
                     :> Header "Authorization" UserToken                     
                     :> Get '[JSON] Reservation
       -- 15 --            
  :<|> "reservation" :> "guestArrived"
                     :> Capture "reservationId" ReservId
                     :> Header "Authorization" UserToken
                     :> Put '[JSON] ()             
       -- 16 --            
  :<|> "reservations" :> "validAbsent"
                      :> Capture "propertyId" PropertyId
                      :> QueryParam "from" Int
                      :> QueryParam "size" Int
                      :> Header "Authorization" UserToken
                      :> Get '[JSON] [Reservation]             
       -- 17 --                 
  :<|> "reservation" :> "availableRooms"
                     :> Capture "reservationId" ReservId  
                     :> Header "Authorization" UserToken 
                     :> Get '[JSON] [BookAvailability]           
 
                     
       



