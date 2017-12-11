{-# LANGUAGE TypeOperators #-}

module Main  where

import           Codec.Picture
import qualified Codec.Picture.Extra    as E
import qualified Codec.Picture.Metadata as M
import           Codec.Picture.Types    as M
import           Control.Error
import           Control.Monad.ST
import           Data.Foldable
import           Data.Semigroup         ((<>))
import           Options.Applicative
import           Scaler16
import           System.Directory       (createDirectoryIfMissing)
import           System.Environment     (getArgs)
import           System.FilePath
import           System.FilePath.Glob
import           System.Posix.Files     (fileExist)

main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (appOpts <**> helper)
      ( fullDesc
     <> progDesc "Scales and compresses images for web, and produces thumbnails"
     <> header "juicy-exe -- Image processor for Hakyll static web" )

main' :: AppOptions -> IO ()
main' (AppOptions q le src dest) = do
  createDirectoryIfMissing False dest
  getglobbed <- glob (src ++ "**/*.jpg")
  for_ getglobbed (processImage q le dest)
  return ()

data AppOptions = AppOptions {
    quality     :: Int
  , longedge    :: Int
  , source      :: FilePath
  , destination :: FilePath
}

appOpts :: Parser AppOptions
appOpts = AppOptions
      <$> option auto
         ( long "quality"
        <> short 'q'
        <> help "Quality of output JPEG (percentage)"
        <> showDefault
        <> value 40
        <> metavar "INT" )
      <*> option auto
         ( long "longedge"
        <> short 'l'
        <> help "Length of longest edge"
        <> showDefault
        <> value 2000
        <> metavar "INT" )
      <*> strOption
          ( long "source"
         <> short 's'
         <> metavar "SOURCE"
         <> help "Directory where images to be processed are found" )
      <*> strOption
          ( long "destination"
         <> short 'd'
         <> metavar "TARGET"
         <> help "Directory to write the resized files and thumbnails" )

-- calc needs to be done as Rationals and then converted back to Ints
chooseNewDimensions :: Int -> (Int, Int) -> (Int, Int)
chooseNewDimensions le (w,h) =
  toInts $ scaleTo (toRational le) (toRational w, toRational  h)
  where
    toInts (w,h) = (floor w, floor h)
    scaleTo le (w,h) =
      let ratio = le / max w h
      in (w * ratio, h * ratio)

processImage :: Int -> Int -> FilePath -> FilePath -> IO ()
processImage quality le dest imagePath = do
  possiblyImage <- readImage imagePath

  case possiblyImage of
    (Right dynImg) -> do
      -- break down the FilePath of the image given and prepare all the bits needed for saving
      let galleryName = last $ splitDirectories $ fst $ splitFileName imagePath -- "/a/b/c/d/foo.jpg" -> "d"
      let imageBaseName = takeBaseName imagePath
      let imageExtension = takeExtension imagePath
      let imageDest  = dest </> galleryName
      let thumbsDest = imageDest </> "thumbs"

      -- make the target directories if necessary
      createDirectoryIfMissing False imageDest
      createDirectoryIfMissing False thumbsDest

      -- generate the target filepaths so that we can check if they need to be converted
      let (nw, nh) = chooseNewDimensions le $ getWidthHeight dynImg
      let (tw, th) = chooseNewDimensions 320 $ getWidthHeight dynImg
      let imagePath = imageDest </> imageBaseName ++ "_" ++ show nw ++ "x" ++ show nh ++ imageExtension
      let thumbPath = thumbsDest </> imageBaseName ++ "_thumb" ++ imageExtension

      -- resize for web and for thumbnail and save resulting images
      resizeAndSave quality imagePath (nw,nh) dynImg
      resizeAndSave 80 thumbPath (tw,th) dynImg

    (Left err) -> print ("Error reading image: " ++ err)

resizeAndSave :: Int -> FilePath -> (Int, Int) -> DynamicImage -> IO ()
resizeAndSave q dest wh img = do
  alreadyExists <- fileExist dest
  if not alreadyExists
  then saveJpgImage q dest $ resize wh img
  else print ("file already present (" ++ dest ++ ") skipping")

getWidthHeight :: DynamicImage -> (Int, Int)
getWidthHeight (ImageRGB8 (Image w h _))   = (w,h)
getWidthHeight (ImageYCbCr8 (Image w h _)) = (w,h)
getWidthHeight (ImageCMYK8 (Image w h _))  = (w,h)
getWidthHeight (ImageRGB16 (Image w h _))  = (w,h)
getWidthHeight (ImageCMYK16 (Image w h _)) = (w,h)
-- following provided for totality but won't work with conversion/scaling
getWidthHeight (ImageY8 (Image w h _))     = (w,h)
getWidthHeight (ImageY16 (Image w h _))    = (w,h)
getWidthHeight (ImageYF (Image w h _))     = (w,h)
getWidthHeight (ImageYA8 (Image w h _))    = (w,h)
getWidthHeight (ImageYA16 (Image w h _))   = (w,h)
getWidthHeight (ImageRGBF (Image w h _))   = (w,h)
getWidthHeight (ImageRGBA8 (Image w h _))  = (w,h)
getWidthHeight (ImageRGBA16 (Image w h _)) = (w,h)


resize :: (Int,Int) -> DynamicImage -> DynamicImage
resize (w,h) (ImageRGB8 image)  = ImageRGB8 $ E.scaleBilinear w h image
resize (w,h) (ImageRGB16 image) = ImageRGB16 $ scaleBilinear16 w h image

resize (w,h) (ImageYCbCr8 image) = ImageRGB8 $ E.scaleBilinear w h (convertImage image)
resize (w,h) (ImageCMYK8 image)  = ImageRGB8 $ E.scaleBilinear w h (convertImage image)
resize (w,h) (ImageCMYK16 image) = ImageRGB16 $ scaleBilinear16 w h (convertImage image)

resize _ other = other -- file is unchanged if it's a format we can't resize TODO
