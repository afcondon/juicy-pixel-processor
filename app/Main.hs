{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main  where

import Codec.Picture
import Codec.Picture.Types as M
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import qualified Codec.Picture.Metadata as M
import qualified Codec.Picture.Extra as E
import Control.Error
import Control.Monad.ST
import Scaler16


main :: IO ()
main = do
  [quality, longEdge, path] <- getArgs
  let le = read longEdge
  let q  = read quality
  dynImg <- readImage path

  createDirectoryIfMissing False $ (takeDirectory path) </> "thumbs"

  case resize le dynImg of
    (Right img) -> saveJpgImage q (scaledFilename img path) img
    (Left err) -> print ("Error scaling image" ++ err)

  case resize 320 dynImg of
    (Right img) -> saveJpgImage 80 (thumbFilename img path) img
    (Left err) -> print ("Error scaling thumbnail" ++ err)
  return ()

scaledFilename :: DynamicImage -> FilePath -> FilePath
scaledFilename img path = dir </> name ++ "_" ++ show w ++ "x" ++ show h ++ ext
  where (w,h) = getWidthHeight img
        (dir, name, ext) = simpleSplitPath path

thumbFilename :: DynamicImage -> FilePath -> String
thumbFilename img path = dir </> "thumbs" </> name ++ "_thumb" ++ ext
  where (dir, name, ext) = simpleSplitPath path

simpleSplitPath :: FilePath -> (String, String, String)
simpleSplitPath p = (takeDirectory p, takeBaseName p, takeExtension p)

getWidthHeight :: DynamicImage -> (Int, Int)
getWidthHeight (ImageRGB8 (Image w h _)) = (w,h)
getWidthHeight (ImageYCbCr8 (Image w h _)) = (w,h)
getWidthHeight (ImageCMYK8 (Image w h _)) = (w,h)
getWidthHeight (ImageRGB16 (Image w h _)) = (w,h)
getWidthHeight (ImageCMYK16 (Image w h _)) = (w,h)
-- following provided for totality but won't work with conversion/scaling
getWidthHeight (ImageY8 (Image w h _)) = (w,h)
getWidthHeight (ImageY16 (Image w h _)) = (w,h)
getWidthHeight (ImageYF (Image w h _)) = (w,h)
getWidthHeight (ImageYA8 (Image w h _)) = (w,h)
getWidthHeight (ImageYA16 (Image w h _)) = (w,h)
getWidthHeight (ImageRGBF (Image w h _)) = (w,h)
getWidthHeight (ImageRGBA8 (Image w h _)) = (w,h)
getWidthHeight (ImageRGBA16 (Image w h _)) = (w,h)


resize :: Int -> Either String DynamicImage -> Either String DynamicImage
resize longEdge (Right (ImageRGB8 image))
  = Right $ ImageRGB8 $ resize' longEdge image
resize longEdge (Right (ImageYCbCr8 image))
  = Right $ ImageRGB8 $ resize' longEdge (convertImage image)
resize longEdge (Right (ImageCMYK8 image))
  = Right $ ImageRGB8 $ resize' longEdge (convertImage image)

resize longEdge (Right (ImageRGB16 image))
  = Right $ ImageRGB16 $ resize16' longEdge image
resize longEdge (Right (ImageCMYK16 image))
  = Right $ ImageRGB16 $ resize16' longEdge (convertImage image)

resize _ _ = Left "unsupported image format"

resize' :: Int -> Image PixelRGB8 -> Image PixelRGB8
resize' longEdge image@(Image w h _) =
  let (nw,nh) = chooseNewDimensions longEdge w h
  in E.scaleBilinear nw nh image

resize16' :: Int -> Image PixelRGB16 -> Image PixelRGB16
resize16' longEdge image@(Image w h _) =
  let (nw,nh) = chooseNewDimensions longEdge w h
  in scaleBilinear16 nw nh image

chooseNewDimensions :: Int -> Int -> Int -> (Int, Int)
chooseNewDimensions longEdge w h =
  toInts $ chooseR le $ toRationals (w,h)
  where
    le = toRational longEdge
    toInts (w,h) = (floor w, floor h)
    toRationals (w,h) = (toRational w, toRational h)
    chooseR le (w,h) = (w * ratio, h * ratio) -- calc needs to be done as Rationals
      where ratio = le / max w h
