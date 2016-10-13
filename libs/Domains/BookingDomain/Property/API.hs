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

module Domains.BookingDomain.Property.API
       (
         PropertyAPI
       ) where

import Domains.BookingDomain.Property.DataTypes
import Servant.API
import Codec.Picture.Types
import Servant.JuicyPixels
import qualified Data.Time as TM
import qualified Data.Text as T

type UserToken = T.Text
type Name = T.Text
type PropertyId = T.Text
type PropType = T.Text
type ImageId = T.Text
type Description = T.Text
type Facilities = [T.Text]
type Rules = [T.Text]
type ContactPhones = [T.Text]
type ProfileName = T.Text

type PropertyAPI =
       -- 1 --
       "property" :> Capture "propertyId" PropertyId
                  :> Get '[JSON] PropertyWithImgIds             
       -- 2 --
  :<|> "property" :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] BasicPropData
                  :> PostCreated '[JSON] Property     

       -- 3 --           
  :<|> "property" :> "location"
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] Region
                  :> Put '[JSON] ()      
       -- 4 --
  :<|> "property" :> "name"
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] Name
                  :> Put '[JSON] ()       
       -- 5 --
  :<|> "property" :> "type"
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] PropType
                  :> Put '[JSON] ()                              
       -- 6 --
  :<|> "property" :> "descriptionES"
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] Description
                  :> Put '[JSON] ()      
       -- 7 --           
  :<|> "property" :> "descriptionEN"
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] Description
                  :> Put '[JSON] ()
       -- 8 -- 
  :<|> "property" :> "facilities"
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] Facilities
                  :> Put '[JSON] ()
       -- 9 -- 
  :<|> "property" :> "rules"
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] Rules
                  :> Put '[JSON] ()
       -- 10 -- 
  :<|> "property" :> "contactPhones"
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] ContactPhones
                  :> Put '[JSON] ()      
       -- 11 --
  :<|> "property" :> "mainImg"
                  :> Capture "propertyId" PropertyId
                  :> Capture "imgId" ImageId
                  :> Header "Authorization" UserToken           
                  :> Put '[JSON] ()
       -- 12 --
  :<|> "property" :> "postImg" 
                  :> Capture "propertyId" PropertyId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JPEG 100] DynamicImage
                  :> PostCreated '[JSON] ImageId              
       -- 13 --
  :<|> "property" :> "deleteImg"
                  :> Capture "propertyId" PropertyId
                  :> Capture "imgId" ImageId
                  :> Header "Authorization" UserToken
                  :> Delete '[JSON] ()
       -- 14 --            
  :<|> "property" :> "imageIds"
                  :> Capture "propertyId" PropertyId
                  :> Get '[JSON] [ImageId]              
       -- 15 --
  :<|> "properties" :> QueryParam "from" Int
                    :> QueryParam "size" Int
                    :> Get '[JSON] [Property]
       -- 16 --
  :<|> "properties" :> "admin"
                    :> Header "Authorization" UserToken
                    :> Get '[JSON] [Property]      
       -- 17 --
  :<|> "properties" :> "search"
                    :> Capture "countryCode" T.Text
                    :> QueryParam "queryString" T.Text
                    :> QueryParam "from" Int
                    :> QueryParam "size" Int
                    :> Get '[JSON] [Property]
 


