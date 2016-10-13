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

module Domains.SideServices.API ( SideAPI ) where

import Servant.API
import Codec.Picture.Types
import Servant.JuicyPixels
import Domains.SideServices.DataTypes
import qualified Data.Text as T


type ImageId = T.Text

type SideAPI =
       -- 1 --
       "bookable" :> "image"
                  :> Capture "imageId" ImageId
                  :> QueryParam "size" T.Text
                  :> Get '[JPEG 100] DynamicImage              
       -- 2 --           
  :<|> "property" :> "image"
                  :> Capture "imageId" ImageId
                  :> QueryParam "size" T.Text
                  :> Get '[JPEG 100] DynamicImage
       -- 3 --           
  :<|> "user" :> "avatar"
              :> Capture "imageId" ImageId
              :> QueryParam "size" T.Text
              :> Get '[JPEG 100] DynamicImage
       -- 4 --
  :<|> "user" :> "forgotPassword"
              :> "sendEmail"           
              :> QueryParam "lang" T.Text
              :> ReqBody '[JSON] T.Text -- either profileName or mainEmail.
              :> Put '[JSON] ()           
       -- 5 --
  :<|> "user" :> "forgotPassword"
              :> "resetPassword"
              :> ReqBody '[JSON] ResetPassData 
              :> Put '[JSON] ()


