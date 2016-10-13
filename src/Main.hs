{-# LANGUAGE TypeOperators         #-}

module Main where

import Configs.ConfigTypes
import Configs.MySQL
import Configs.ElasticSearch.BookingSystem
import Configs.ElasticSearch.ContentAdmin
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import Servant
import Servant.Server

import Domains.UserDomain.User.API
import Domains.UserDomain.User.Interpreter
import Domains.UserDomain.Review.API
import Domains.UserDomain.Review.Interpreter
import Domains.BookingDomain.Property.API
import Domains.BookingDomain.Property.Interpreter
import Domains.BookingDomain.Bookable.API
import Domains.BookingDomain.Bookable.Interpreter
import Domains.BookingDomain.Reservation.API
import Domains.BookingDomain.Reservation.Interpreter
import Domains.BookingDomain.Cancellation.API
import Domains.BookingDomain.Cancellation.Interpreter
import Domains.SideServices.API
import Domains.SideServices.Interpreter
import Domains.ContentAdminDomain.Article.API
import Domains.ContentAdminDomain.Article.Interpreter


type CombinedAPI = UserAPI 
              :<|> ReviewAPI
              :<|> PropertyAPI 
              :<|> BookableAPI 
              :<|> ReservationAPI 
              :<|> CancellationAPI
              :<|> SideAPI
              :<|> ArticleAPI


combinedServer coMysql coEsBkl coEsArt = (userServer coMysql) 
                                    :<|> (reviewServer coMysql)
                                    :<|> (propertyServer coMysql coEsBkl)
                                    :<|> (bookableServer coMysql coEsBkl)
                                    :<|> (reservationServer coMysql coEsBkl)
                                    :<|> (cancellationServer coMysql coEsBkl)
                                    :<|> (sideServer coMysql)
                                    :<|> (articleServer coEsArt)



getConfigs :: Environment -> (ConfigMySQL, ConfigES, ConfigES)
getConfigs env
  | env == Development = (mysqlDevelConfig, esBookingDevConfig, esArticlesDevConfig)
  | env == Production = (mysqlProdConfig, esBookingProdConfig, esArticlesProdConfig)
  | env == Test = (mysqlTestConfig, esBookingTestConfig, esArticlesTestConfig)


fullAPI :: Proxy CombinedAPI
fullAPI = Proxy 


main :: IO ()
main = do
  env <- getAppEnv
  port <- getAppPort
  let (coMysql, coEsBkl, coEsArt) = getConfigs env
      server = combinedServer coMysql coEsBkl coEsArt
      logger = setApiLogger env
      app = serveWithContext fullAPI (basicAuthServerContext coMysql) server
  run port $ logger app



  






  
