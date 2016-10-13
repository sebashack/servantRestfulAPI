module HelperLibs.MySQL.SideServicesRepo where

import Control.Monad (replicateM)
import System.Random
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as TE


-- Generate a recovery code.
genRecoveryCode :: IO ByteString
genRecoveryCode  = do
  part1 <- replicateM 2 (randomRIO ('a', 'z'))
  part2 <- replicateM 2 (randomRIO ('0', '9'))
  part3 <- replicateM 2 (randomRIO ('A', 'Z'))
  part4 <- replicateM 2 (randomRIO ('a', 'z'))
  part5 <- replicateM 2 (randomRIO ('0', '9'))
  part6 <- replicateM 2 (randomRIO ('A', 'Z'))
  part7 <- replicateM 2 (randomRIO ('a', 'z'))
  part8 <- replicateM 2 (randomRIO ('0', '9'))
  part9 <- replicateM 2 (randomRIO ('A', 'Z'))
  return $ TE.encodeUtf8 $ T.pack (part1 ++
                                   part2 ++
                                   part3 ++
                                   part4 ++
                                   part5 ++
                                   part6 ++
                                   part7 ++
                                   part8 ++
                                   part9  )

