module Recogs.Util.ImageReader (
                     getImageData
                   ) where

import Codec.Picture
    ( PixelRGB8
    , Image
    , DynamicImage(ImageRGB8, ImageRGBA8, ImageY8, ImageYA8, ImageYCbCr8)
    , readImage
    )
import Codec.Picture.Types
    ( ColorSpaceConvertible(convertImage)
    , ColorConvertible(promoteImage)
    , dropAlphaLayer
    )

getImageData :: FilePath -> IO (Either String (Image PixelRGB8))
getImageData filePath = do
    rez <- readImage filePath
    case rez of
        Left err    -> return $ Left ("\n(X) loading error: (" ++ filePath ++ ")" ++ err)
        Right image -> return $ Right $ case image of
                         ImageYCbCr8 img -> convertImage img
                         ImageY8 img     -> promoteImage img
                         ImageYA8 img    -> promoteImage img
                         ImageRGB8 img   -> promoteImage img
                         ImageRGBA8  img -> convertImage $ dropAlphaLayer img

