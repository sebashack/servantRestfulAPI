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

module Domains.UserDomain.Review.Interpreter ( reviewServer ) where

import Data.ByteString.Char8 (pack)
import Control.Monad.Except
import Network.Wai
import Servant
import Domains.UserDomain.Review.API
import Configs.ConfigTypes
import HelperLibs.MySQL.ActionRunner
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import HelperLibs.Interpreters.BookingDomain
import HelperLibs.Interpreters.Review
import qualified Data.ByteString.Base64 as B64
import qualified Database.Persist.MySQL as MySQL
import qualified Domains.UserDomain.Review.DataTypes as RvT
import qualified Schemas.SQL.Reservation as RSC
import qualified Schemas.SQL.DbTypes as DbT
import qualified Repositories.ReviewRepo.Operations as RvR
import qualified Repositories.ReservationRepo.Operations as RR
import qualified Data.ByteString.Lazy as LB (toStrict, fromStrict)
import qualified Data.ByteString as SB (length, ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Data.Text.Encoding as TE


postReview :: ConfigMySQL 
           -> Integer
           -> RvT.ReviewData
           -> Maybe T.Text
           -> ExceptT ServantErr IO RvT.Review
postReview _ _ _ Nothing = throwError _noToken
postReview config reservId reviewData (Just token) = do
  review <- liftIO $ runMySQL config $ do
    let reviewValidation = validateReviewData reviewData 
    userValidation <- validateUser token 
    case (userValidation, reviewValidation) of
      (Left error, _) -> return $ Left error
      (_, Left error) -> return $ Left error
      (Right (userId, _, _, _, _), Right ()) -> do
        eitherReserv <- RR.getReservById reservId
        case eitherReserv of
          Left error -> return reservNotFound
          Right repoReserv -> do
            let (RSC.Reservation byAdmin userKey _ _ _ _ _ state _ _ _ _ _ cOut _ _ _) = repoReserv
                userId' = fmap userKeyToInt userKey
            currentDate <- liftIO $ TM.getCurrentTime
            reviewed <- RvR.getReservReview reservId
            case (byAdmin, Just userId == userId', state == DbT.Accepted, currentDate >= cOut, reviewed) of
              (True, _, _, _, _) -> return cannotReviewByAdmin
              (_, False, _, _, _) -> return userReservMismatch
              (_, _, False, _, _) -> return cannotReviewNotAccepted
              (_, _, _, False, _) -> return cannotReviewBeforeCheckOut
              (_, _, _, _, Right _) -> return alreadyReviewed
              (False, True, True, True, Left _) -> do
                (_, repoReview) <- RvR.storeReview reservId score content
                let reviewDate = RSC.reviewReviewDate repoReview
                return $ Right (RvT.Review reservId reviewDate reviewData)
  either (\err -> throwError err) (\r -> return r) (getCustomError review)
  where
    (RvT.ReviewData content score) = reviewData




getPropReviews :: ConfigMySQL 
               -> T.Text
               -> Maybe Int
               -> Maybe Int
               -> ExceptT ServantErr IO [RvT.Review]
getPropReviews config propId from size = do
  reviews <- liftIO $ runMySQL config $ RvR.getPropertyReviews propId from' size'
  return $ fmap toDomainReview reviews 
  where
    from' = toInteger $ maybe 0 (setBound 0) from
    size' = toInteger $ maybe 15 (setBound 15) size


getReservReview :: ConfigMySQL 
                -> Integer
                -> ExceptT ServantErr IO RvT.Review
getReservReview config reservId = do
  review <- liftIO $ runMySQL config $ do
    eitherReview <- RvR.getReservReview reservId
    case eitherReview of
      Left error -> return $ reviewNotFound
      Right repoReview -> return $ Right (toDomainReview repoReview)
  either (\err -> throwError err) (\r -> return r) (getCustomError review)   

    

getUserReviews :: ConfigMySQL
               -> Maybe T.Text
               -> Maybe Int
               -> Maybe Int
               -> ExceptT ServantErr IO [RvT.Review]
getUserReviews _ Nothing _ _ = throwError _noToken              
getUserReviews config (Just token) from size = do
  reviews <- liftIO $ runMySQL config $ do
    let credentials = getTokenUserIdAndProfName $ T.append "Bearer "  token
    case credentials of
      Left error -> return $ Left error
      Right (userId, _, _) -> do
        repoReviews <- RvR.getUserReviews userId from' size'
        return $ Right (fmap toDomainReview repoReviews)
  either (\err -> throwError err) (\r -> return r) (getCustomError reviews)
  where
    from' = toInteger $ maybe 0 (setBound 0) from
    size' = toInteger $ maybe 15 (setBound 15) size

                                     
reviewServer :: ConfigMySQL -> Server ReviewAPI
reviewServer config = (getPropReviews config)
                 :<|> (postReview config)
                 :<|> (getReservReview config)
                 :<|> (getUserReviews config)










