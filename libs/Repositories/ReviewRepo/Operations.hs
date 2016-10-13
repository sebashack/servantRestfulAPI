{-# LANGUAGE OverloadedStrings #-}

module Repositories.ReviewRepo.Operations where


import Data.Time
import HelperLibs.MySQL.ReviewRepo 
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Schemas.SQL.DbTypes as DbT
import qualified Data.Time as TM
import qualified Schemas.SQL.Reservation as SCH
import qualified Schemas.SQL.User as USCH
import qualified Database.Esqueleto  as DBEsq
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Persist as Per



-- Given the basic information for a review, store that review in DB. 
storeReview :: MonadIO m => Integer
                         -> Int
                         -> T.Text
                         -> ReaderT MySQL.SqlBackend m (Integer, SCH.Review) 
storeReview reservId score content = do
  currentDate <- liftIO TM.getCurrentTime   
  let reservKey =  SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)
      review = SCH.Review currentDate
                          reservKey
                          content
                          score
  reviewKey <- Per.insert review
  let reviewId = toInteger $ MySQL.unSqlBackendKey (SCH.unReviewKey reviewKey)
  return (reviewId, review)



-- Given a reservationId get its review.
getReservReview :: MonadIO m => Integer -> ReaderT MySQL.SqlBackend m (Either String (Integer, SCH.Review))
getReservReview reservId = do
  let reservKey =  SCH.ReservationKey $ MySQL.SqlBackendKey (fromInteger reservId)
  maybeReview <- Per.getBy $ SCH.UniqueReservReview reservKey
  case maybeReview of
    Nothing -> return $ Left ("There's no review belonging to reservation with Id " ++ (show reservId))
    Just entity -> return $ Right (reviewEntityToTuple entity) 



-- Given a propertId get all reviews belonging to that property
-- The reviews are ordered from the most recent to the oldest.
getPropertyReviews :: MonadIO m => T.Text
                                -> Integer
                                -> Integer
                                -> ReaderT MySQL.SqlBackend m [(Integer, SCH.Review)]  
getPropertyReviews propId from size = do
   reviews <- (DBEsq.select $ DBEsq.from (\(reviews `DBEsq.InnerJoin` reservs) -> do
                (DBEsq.offset $ fromInteger from)
                (DBEsq.limit $ fromInteger size)
                (DBEsq.orderBy [DBEsq.desc (reviews DBEsq.^. SCH.ReviewReviewDate)])
                DBEsq.on (reviews DBEsq.^. SCH.ReviewReservId  
                          DBEsq.==. reservs DBEsq.^. SCH.ReservationId)
                DBEsq.where_ (reservs DBEsq.^. SCH.ReservationPropertyId 
                              DBEsq.==. DBEsq.val (TE.encodeUtf8 propId))
                return reviews))
   return $ fmap reviewEntityToTuple reviews




-- Given a propertId get all review scores belonging to that property
-- It is important to note that this function returns the scores from most recent to oldest,
-- and you can pick the size of the score list so that you can compute a reasonable scoreMean.
getPropertyScores :: MonadIO m => T.Text
                               -> Integer
                               -> ReaderT MySQL.SqlBackend m [Float]  
getPropertyScores propId size = do
   scores <- (DBEsq.select $ DBEsq.from (\(reviews `DBEsq.InnerJoin` reservs) -> do
               (DBEsq.offset $ fromInteger 0)
               (DBEsq.limit $ fromInteger size)
               (DBEsq.orderBy [DBEsq.desc (reviews DBEsq.^. SCH.ReviewReviewDate)])
               DBEsq.on (reviews DBEsq.^. SCH.ReviewReservId  
                         DBEsq.==. reservs DBEsq.^. SCH.ReservationId)
               DBEsq.where_ (reservs DBEsq.^. SCH.ReservationPropertyId 
                             DBEsq.==. DBEsq.val (TE.encodeUtf8 propId))
               return $ reviews DBEsq.^. SCH.ReviewScore))
   return $ fmap (\(DBEsq.Value v) -> fromIntegral v) scores





-- Given a userId get all reviews belonging to that user
-- The reviews are ordered from the most recent to the oldest.
getUserReviews :: MonadIO m => Integer
                            -> Integer
                            -> Integer
                            -> ReaderT MySQL.SqlBackend m [(Integer, SCH.Review)]  
getUserReviews userId from size = do
  let userKey = USCH.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
  reviews <- (DBEsq.select $ DBEsq.from (\(reviews `DBEsq.InnerJoin` reservs `DBEsq.InnerJoin` users) -> do
               (DBEsq.offset $ fromInteger from)
               (DBEsq.limit $ fromInteger size)
               (DBEsq.orderBy [DBEsq.desc (reviews DBEsq.^. SCH.ReviewReviewDate)])
               DBEsq.on (DBEsq.just (users DBEsq.^.USCH.UserId)
                         DBEsq.==. reservs DBEsq.^. SCH.ReservationUserId)
               DBEsq.on (reservs DBEsq.^. SCH.ReservationId
                         DBEsq.==. reviews DBEsq.^. SCH.ReviewReservId)
               DBEsq.where_ (users DBEsq.^. USCH.UserId
                             DBEsq.==. DBEsq.val userKey)
               return reviews))                         
  return $ fmap reviewEntityToTuple reviews




