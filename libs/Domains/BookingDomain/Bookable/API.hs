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

module Domains.BookingDomain.Bookable.API 
       (
        BookableAPI
       ) where


import Domains.BookingDomain.Bookable.DataTypes
import Servant.API
import Codec.Picture.Types
import Servant.JuicyPixels
import qualified Data.Time as TM
import qualified Data.Text as T

type UserToken = T.Text
type BookableId = T.Text
type Name = T.Text
type Description = T.Text
type MaxOccupancy = Int
type NumUnits = Int
type PricingId = T.Text
type PropertyId = T.Text
type ImageId = T.Text


type BookableAPI =
       -- 1 --
       "bookable" :> Capture "bookableId" BookableId
                  :> Get '[JSON] Bookable               
       -- 2 --
  :<|> "bookable" :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] BasicBookableData
                  :> PostCreated '[JSON] Bookable            
       -- 3 --
  :<|> "bookable" :> "list"
                  :> Capture "bookableId" BookableId
                  :> Header "Authorization" UserToken
                  :> Put '[JSON] ()
       -- 4 --
  :<|> "bookable" :> "unlist"
                  :> Capture "bookableId" BookableId
                  :> Header "Authorization" UserToken
                  :> Put '[JSON] ()             
       -- 5 -- 
  :<|> "bookable" :> Capture "bookableId" BookableId
                  :> ReqBody '[JSON] UserCredentials
                  :> Header "Authorization" UserToken
                  :> Delete '[JSON] ()            
       -- 6 --
  :<|> "bookable" :> "descriptionES"
                  :> Capture "bookableId" BookableId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] Description
                  :> Put '[JSON] ()
       -- 7 --
  :<|> "bookable" :> "descriptionEN"
                  :> Capture "bookableId" BookableId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] Description
                  :> Put '[JSON] ()         
       -- 8 --
  :<|> "bookable" :> "specs"
                  :> Capture "bookableId" BookableId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] BookableSpecs
                  :> Put '[JSON] ()             
       -- 9 -- 
  :<|> "bookable" :> "maxOccupancy"
                  :> Capture "bookableId" BookableId
                  :> Capture "maxOccupancy" MaxOccupancy
                  :> Header "Authorization" UserToken
                  :> Put '[JSON] ()                
       -- 10 -- 
  :<|> "bookable" :> "addRooms"
                  :> Capture "bookableId" BookableId
                  :> ReqBody '[JSON] [T.Text] 
                  :> Header "Authorization" UserToken
                  :> Put '[JSON] ()               
       -- 11 -- 
  :<|> "bookable" :> "removeRooms"
                  :> Capture "bookableId" BookableId
                  :> ReqBody '[JSON] [T.Text] 
                  :> Header "Authorization" UserToken
                  :> Put '[JSON] ()                                      
       -- 12 --
  :<|> "bookable" :> "addPricing"
                  :> Capture "bookableId" BookableId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] PricingData
                  :> Put '[JSON] ()
       -- 13 --
  :<|> "bookable" :> "editPricing"
                  :> Capture "bookableId" BookableId
                  :> Capture "pricingId" PricingId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JSON] PricingData
                  :> Put '[JSON] ()
       -- 14 --
  :<|> "bookable" :> "removePricing"
                  :> Capture "bookableId" BookableId
                  :> Capture "pricingId" PricingId
                  :> Header "Authorization" UserToken
                  :> Put '[JSON] ()             
       -- 15 --
  :<|> "property" :> "bookables"
                  :> Capture "propertyId" PropertyId
                  :> Get '[JSON] [Bookable]
       -- 16 --
  :<|> "property" :> "AvBookables"
                  :> Capture "propertyId" PropertyId
                  :> ReqBody '[JSON] CheckInOut
                  :> Get '[JSON] [SearchResultWithImgIds]
       -- 17 --           
  :<|> "bookables" :> "basicSearch"
                   :> QueryParam "from" Int
                   :> QueryParam "size" Int
                   :> ReqBody '[JSON] BasicSearch
                   :> Get '[JSON] [SearchResult]
       -- 18 --           
  :<|> "bookables" :> "advancedSearch"
                   :> QueryParam "from" Int
                   :> QueryParam "size" Int
                   :> ReqBody '[JSON] AdvancedSearch
                   :> Get '[JSON] [SearchResult]         
       -- 19 --
  :<|> "bookable" :> "postImg" 
                  :> Capture "bookableId" BookableId
                  :> Header "Authorization" UserToken
                  :> ReqBody '[JPEG 100] DynamicImage
                  :> Post '[JSON] ImageId               
       -- 20 --            
  :<|> "bookable" :> "imageIds"
                  :> Capture "bookableId" BookableId
                  :> Get '[JSON] [ImageId]                                 
       -- 21 --
  :<|> "bookable" :> "deleteImg"
                  :> Capture "bokableId" BookableId
                  :> Capture "imgId" ImageId
                  :> Header "Authorization" UserToken
                  :> Delete '[JSON] ()
       -- 22 --
  :<|> "bookable" :> "period"
                  :> Capture "bookableId" BookableId                  
                  :> ReqBody '[JSON] Period
                  :> Header "Authorization" UserToken
                  :> Get '[JSON] Report
       -- 23 --
  :<|> "bookable" :> "incomeInfo"
                  :> Capture "bookableId" BookableId
                  :> ReqBody '[JSON] Period
                  :> Header "Authorization" UserToken
                  :> Get '[JSON] EconomicReport                
       -- 24 --           
  :<|> "bookable" :> "applyDiscount" 
                  :> Capture "bookableId" BookableId
                  :> Capture "discount" Int
                  :> Header "Authorization" UserToken
                  :> Put '[JSON] ()     
       -- 25 --              
  :<|> "property" :> "periodReport"
                  :> Capture "propertyId" PropertyId                  
                  :> ReqBody '[JSON] Period
                  :> Header "Authorization" UserToken
                  :> Get '[JSON] [PeriodReport]       
                    
 






