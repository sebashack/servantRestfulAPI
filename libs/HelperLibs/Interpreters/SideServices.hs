{-# LANGUAGE OverloadedStrings #-}

module HelperLibs.Interpreters.SideServices where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import HelperLibs.Interpreters.BookingDomain
import qualified Data.ByteString as SB (ByteString)
import qualified Data.Text as T




returnDecodedImg :: Either String SB.ByteString
                 -> Maybe T.Text
                 -> Either String DynamicImage
returnDecodedImg eitherImg imgSize = 
  case eitherImg of
    Left error -> imgNotFound
    Right binary -> 
      case decodeJpeg binary of
        Left error -> decodeImgError
        Right image -> 
          let imgRGB8 = convertRGB8 image
              scaledImg = scaleImg imgSize imgRGB8
          in Right $ ImageRGB8 scaledImg
  where
    scaleImg maybeSize img = case maybeSize of
      Just "mini" -> scaleBilinear 100 100 img
      Just "avatar" -> scaleBilinear 200 200 img
      Just "bigAvatar" -> scaleBilinear 250 250 img
      Just "medium" -> scaleBilinear 530 250 img
      Just "standard"-> scaleBilinear 840 460 img
      Just "large" -> scaleBilinear 920 540 img
      _ -> img
