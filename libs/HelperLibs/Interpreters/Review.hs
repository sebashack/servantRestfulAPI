module HelperLibs.Interpreters.Review where

import HelperLibs.Interpreters.BookingDomain
import qualified Data.Text as T
import qualified Domains.UserDomain.Review.DataTypes as RvT
import qualified Schemas.SQL.Reservation as RSC



validateReviewData :: RvT.ReviewData -> Either String ()
validateReviewData (RvT.ReviewData content score)
  | numChars > 1300 = badReviewContent
  | numChars < 80 = badReviewContent
  | score < 1 = badReviewScore
  | score > 10 = badReviewScore
  | otherwise = Right ()
  where
    numChars = T.length content

toDomainReview :: (Integer, RSC.Review) -> RvT.Review
toDomainReview (rwId, RSC.Review rwDate reservKey content score) =
  RvT.Review (reservKeyToInt reservKey) rwDate (RvT.ReviewData  content score)
  


