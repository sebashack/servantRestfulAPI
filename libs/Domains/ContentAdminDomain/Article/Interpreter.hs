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

module Domains.ContentAdminDomain.Article.Interpreter ( articleServer ) where


import Control.Monad.Except
import Network.Wai
import Servant
import Data.Aeson
import Data.Aeson.Types
import Domains.ContentAdminDomain.Article.API
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad (mapM)
import Data.Monoid
import HelperLibs.Interpreters.ContentAdmin
import HelperLibs.ElasticSearch.ResponseParser
import qualified Repositories.ContentAdminRepo.Operations as CAR 
import qualified Domains.ContentAdminDomain.Article.DataTypes as AT
import qualified Configs.ConfigTypes as CT
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE



-- Given an articleId and a locale (es, esp, en, eng) this endpoint searches that article either in esp_article
-- type or in eng_article type. Also note that at most the 10 most relevant abstracts of articles related with
-- the queried article are also returned. 
getArticle :: CT.ConfigES
           -> T.Text
           -> Maybe T.Text
           -> ExceptT ServantErr IO AT.ArticleWithAbstracts
getArticle config artId lang = do
  eitherArticle <- liftIO $ runExceptT $ CAR.queryArticleById config artId lang'
  case eitherArticle of
    Left error -> throwError _articleNotFound
    Right (Object object1) ->
      case parseArticle object1 of
        Nothing -> throwError _parseArtErr
        Just article -> do
          let artTitle = AT.title article
              artId = AT.artId article
          eitherAbstracts <- liftIO $ runExceptT $ CAR.queryArticlesByText config lang' artTitle 0 12
          case eitherAbstracts of
            Left error -> throwError _absNotFound
            Right (Object object2) -> maybe (throwError _parseAbsErr)
                                            (\abs -> return $ AT.ArticleWithAbstracts article (rmvDup artId abs))
                                            (parseAbstracts object2) 
  where
    lang' = maybe False (\l -> if l == "esp" || l == "es" then True else False) lang
    rmvDup artId abstracts =
      let absId (AT.Abstract v _ _ _ _ _ _ _ _) = v
      in take 10 $ filter (\a -> absId a /= artId ) abstracts
        


-- Given a locale (es, esp, en, eng) this endpoint returns the especified number of article
-- abstracts, according to from and size, with defaults 0 and 15. 
getAbstracts :: CT.ConfigES
             -> Maybe T.Text
             -> Maybe Int
             -> Maybe Int
             -> ExceptT ServantErr IO [AT.Abstract]
getAbstracts config lang from size = do
  eitherAbstracts <- liftIO $ runExceptT $ CAR.queryArticles config lang' from' size'
  case eitherAbstracts of
    Left error -> throwError _absNotFound
    Right (Object object) -> maybe (throwError _parseAbsErr)
                                   (\abs -> return abs)
                                   (parseAbstracts object)
  where
    lang' = maybe False (\l -> if l == "esp" || l == "es" then True else False) lang
    from' = fromIntegral $ maybe 0 (setBound 0) from
    size' = fromIntegral $ maybe 15 (setBound 15) size


-- Given a text query this endpoing returns the most relevant article abstracts which match
-- the query. Pagination is available with from and size arguments.
getAbstractsByWords :: CT.ConfigES
                    -> Maybe T.Text
                    -> Maybe T.Text
                    -> Maybe Int
                    -> Maybe Int
                    -> ExceptT ServantErr IO [AT.Abstract]
getAbstractsByWords config lang text from size = do
  eitherAbstracts <- liftIO $ runExceptT $ CAR.queryArticlesByText config lang' text' from' size'
  case eitherAbstracts of
    Left error -> throwError _absNotFound
    Right (Object object) -> maybe (throwError _parseAbsErr)
                                   (\abs -> return abs)
                                   (parseAbstracts object)
  where
    text' = maybe "" id text
    lang' = maybe False (\l -> if l == "esp" || l == "es" then True else False) lang
    from' = fromIntegral $ maybe 0 (setBound 0) from
    size' = fromIntegral $ maybe 15 (setBound 15) size



-- Given a country code filter and a region and city text queries, this endpoints filters article
-- abstracts by country code and returns the most relevant article abstracts which match either the region
-- or the city queries. Pagination is available with from and size arguments.
getAbstractsByLoc :: CT.ConfigES
                  -> T.Text
                  -> Maybe T.Text
                  -> Maybe T.Text
                  -> Maybe T.Text
                  -> Maybe Int
                  -> Maybe Int
                  -> ExceptT ServantErr IO [AT.Abstract]
getAbstractsByLoc config country region city lang from size = do
  eitherAbstracts <- liftIO $ runExceptT $ CAR.queryArticlesByLoc config lang' country region' city' from' size'
  case eitherAbstracts of
    Left error -> throwError _absNotFound
    Right (Object object) -> maybe (throwError _parseAbsErr)
                                   (\abs -> return abs)
                                   (parseAbstracts object)
  where
    region' = maybe "" id region
    city' = maybe "" id city
    lang' = maybe False (\l -> if l == "esp" || l == "es" then True else False) lang
    from' = fromIntegral $ maybe 0 (setBound 0) from
    size' = fromIntegral $ maybe 15 (setBound 15) size


  
articleServer :: CT.ConfigES -> Server ArticleAPI
articleServer config = (getArticle config)
                  :<|> (getAbstracts config)
                  :<|> (getAbstractsByWords config)
                  :<|> (getAbstractsByLoc config)
