module ImageReader (
                     getImageData
                   ) where
import System.Environment (getArgs)
import Codec.Picture
import Codec.Picture.Types

getImageData :: FilePath -> IO (Maybe (Image PixelRGB8))
getImageData filePath = do
    rez <- readImage filePath
    case rez of
        Left err -> putStr ("\n(X) loading error: (" ++ filePath ++ ")" ++ err) >> return Nothing
        Right (ImageYCbCr8 img) -> return $ Just $ convertImage img
        Right (ImageY8 img)     -> return $ Just $ promoteImage img
        Right (ImageYA8 img)    -> return $ Just $ promoteImage img
        Right (ImageRGB8 img)   -> return $ Just $ promoteImage img
        Right (ImageRGBA8 img)  -> return $ Just $ convertImage $ dropAlphaLayer img

-- inspectify :: Maybe (Image PixelRGB8) -> String
-- inspectify Nothing  = "Nothing to see here..."
-- inspectify (Just i) = "Width: " ++ show (imageWidth i) ++ ", Height: " ++ show (imageHeight i)

-- main :: IO ()
-- main = do
--         args <- getArgs
--         if null args
--         then putStrLn "Please specify an image as a parameter"
--         else do
--                 daten <- getImageData $ head args
--                 putStrLn $ inspectify daten
