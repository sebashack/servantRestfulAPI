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

module Domains.ContentAdminDomain.Article.API 
       (
        ArticleAPI
       ) where


import Domains.ContentAdminDomain.Article.DataTypes
import Servant.API
import Codec.Picture.Types
import Servant.JuicyPixels
import qualified Data.Time as TM
import qualified Data.Text as T

type ArticleId = T.Text

type ArticleAPI =
       -- 1 --
       "article" :> Capture "articleId" ArticleId
                 :> QueryParam "lang" T.Text
                 :> Get '[JSON] ArticleWithAbstracts
       -- 2 --
  :<|> "articles" :> QueryParam "lang" T.Text
                  :> QueryParam "from" Int
                  :> QueryParam "size" Int
                  :> Get '[JSON] [Abstract]           
       -- 3 --
  :<|> "articles" :> "search"
                  :> QueryParam "lang" T.Text
                  :> QueryParam "words" T.Text
                  :> QueryParam "from" Int
                  :> QueryParam "size" Int
                  :> Get '[JSON] [Abstract]
       -- 4 --
  :<|> "articles" :> "location"
                  :> Capture "country" T.Text
                  :> QueryParam "region" T.Text
                  :> QueryParam "city" T.Text
                  :> QueryParam "lang" T.Text
                  :> QueryParam "from" Int
                  :> QueryParam "size" Int
                  :> Get '[JSON] [Abstract]
       
      

-- If no language is specified, the default language will always be english.

