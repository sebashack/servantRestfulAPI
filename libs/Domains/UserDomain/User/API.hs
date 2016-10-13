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

module Domains.UserDomain.User.API
  (
    UserAPI
  ) where

import Domains.UserDomain.User.DataTypes
import Servant.API
import Servant.API.BasicAuth
import Codec.Picture.Types
import Servant.JuicyPixels
import qualified Data.Time as TM
import qualified Data.Text as T

type UserToken = T.Text
type ProfileName = T.Text
type Password = T.Text
type Email = T.Text
type AvatarId = T.Text

type UserAPI =

       -- 1 --
       "user" :> Header "Authorization" UserToken
              :> Get '[JSON] User
       -- 2 --
  :<|> "user" :> ReqBody '[JSON] CreateUserData
              :> PostCreated '[JSON] NewUser           
       -- 3 --
  :<|> "user" :> "basicData"
              :> Header "Authorization" UserToken
              :> ReqBody '[JSON] BasicUserData
              :> Put '[JSON] TokenInfo       
       -- 4 --
  :<|> "user" :> "isAvailable"
              :> "profileName"
              :> Capture "profileName" ProfileName
              :> Get '[JSON] Bool           
       -- 5 --
  :<|> "user" :> "isAvailable"
              :> "email"
              :> Capture "email" Email
              :> Get '[JSON] Bool        
       -- 6 --
  :<|> "user" :> "changePassword"
              :> Header "Authorization" UserToken
              :> ReqBody '[JSON] ChangePassData
              :> Put '[JSON] ()
       -- 7 --
  :<|> "user" :> "avatar"
              :> Header "Authorization" UserToken
              :> ReqBody '[JPEG 100] DynamicImage
              :> PostCreated '[JSON] AvatarId
       -- 8 --
  :<|> "user" :> "avatar"
              :> Header "Authorization" UserToken
              :> Delete '[JSON] ()
       -- 9 -- 
  :<|> "user" :> "getAuthToken"
              :> BasicAuth "credentials" (UserToken, TM.UTCTime)
              :> Put '[JSON] TokenInfo
       -- 10 --
  :<|> "user" :> "invalidateToken"
              :> Header "Authorization" UserToken
              :> Put '[JSON] ()           
       -- 11 --
  :<|> "user" :> "refreshToken"
              :> Header "Authorization" UserToken
              :> Put '[JSON] TokenInfo


