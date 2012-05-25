module ImageReader (
                    getJpegData
                   ) where
import System.Environment (getArgs)
import Codec.Picture
import Codec.Picture.Types
import qualified Data.ByteString as B

getJpegData :: FilePath -> IO (Maybe (Image PixelRGB8))
getJpegData filePath = do
    file <- B.readFile filePath
    rez <- catch (return $ decodeJpeg file)
                 (\err -> return $ Left (show err))
    case rez of
        Left err -> (putStr $ "\n(X) JPEG loading error: (" ++ filePath ++ ")" ++ err) >> return Nothing
        Right (ImageYCbCr8 img) -> return $ Just $ convertImage img
        Right (ImageY8 img) -> return $ Just $ promoteImage img
        Right _ -> (putStr $ "\n(X) JPEG loading error: (" ++ filePath ++ ")") >> return Nothing

-- inspectify :: Maybe (Image PixelRGB8) -> String
-- inspectify Nothing  = "Nix zu sehen..."
-- inspectify (Just i) = "Width: " ++ show (imageWidth i) ++ ", Height: " ++ show (imageHeight i)

-- main :: IO ()
-- main = do
--         args <- getArgs
--         if length args == 0
--         then putStrLn "Please specify a jpg as a parameter"
--         else do
--                 daten <- getJpegData $ head args
--                 putStrLn $ inspectify daten
