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

module Domains.UserDomain.Review.API 
       (
        ReviewAPI
       ) where


import Domains.UserDomain.Review.DataTypes
import Servant.API
import Codec.Picture.Types
import Servant.JuicyPixels
import qualified Data.Time as TM
import qualified Data.Text as T

type PropertyId = T.Text
type ReservId = Integer
type UserToken = T.Text
type ProfileName = T.Text

type ReviewAPI =
        -- 1 --
       "property" :> "reviews"
                  :> Capture "propertyId" PropertyId
                  :> QueryParam "from" Int
                  :> QueryParam "size" Int
                  :> Get '[JSON] [Review]    
       -- 2 --
  :<|> "reservation"  :> "review"
                      :> Capture "reservationId" ReservId
                      :> ReqBody '[JSON] ReviewData
                      :> Header "Authorization" UserToken
                      :> PostCreated '[JSON] Review         
       -- 3 --
  :<|> "reservation" :> "review"
                     :> Capture "reservationId" ReservId
                     :> Get '[JSON] Review
       -- 4 --
  :<|> "userReviews" :> QueryParam "userToken" UserToken
                     :> QueryParam "from" Int
                     :> QueryParam "size" Int       
                     :> Get '[JSON] [Review]
        










